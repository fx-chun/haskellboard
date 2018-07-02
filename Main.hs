{-# LANGUAGE Arrows, MultiWayIf #-}
-- based on example in https://wiki.haskell.org/Yampa/reactimate

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import FRP.Yampa.Geometry

import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Linux.Input.Device
import qualified System.Linux.Input.Event as EvDev
import System.Timeout
import System.Hardware.WiringPi
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan

import Debug.Trace

data Inputs = Inputs { 
  iNewUpdate      :: Event (),
  iDisconnected   :: Event (), 

  iJoystick       :: Event Throttle,
  iBJoystick      :: Event Bool,

  iBUp            :: Event Bool,
  iBDown          :: Event Bool,
  iBLeft          :: Event Bool,
  iBRight         :: Event Bool,
  iBTrigger       :: Event Bool,
  iBShoulder      :: Event Bool
} deriving (Show)

data MaybeRawEvents = InputDisconnected
                    | RawInput EvDev.Event

defaultInputs = Inputs {
  iNewUpdate = NoEvent,
  iDisconnected = NoEvent,

  iJoystick = NoEvent,
  iBJoystick = NoEvent,

  iBUp = NoEvent,
  iBDown = NoEvent,
  iBLeft = NoEvent,
  iBRight = NoEvent,
  iBTrigger = NoEvent,
  iBShoulder = NoEvent
}

newInputs = defaultInputs {
  iNewUpdate = Event ()
}

resetInputs = defaultInputs {
  iDisconnected = Event ()
}

data Outputs = Outputs { 
  oPrintBuffer :: Event [Char],
  oPWMOutput :: PwmValue
} deriving (Show)

defaultOutputs = Outputs {
  oPrintBuffer = NoEvent,
  oPWMOutput = 0
} 

type Throttle = Double

---
pwmPin = Gpio 18
statusLedPin = Gpio 12
pwmClock = 94
pwmRange = 4096 :: PwmValue
---
 rampedThrottleP = 0.30
--throttleStepPerSecond = 0.20
---
cruisingSpeedMaxOutput = 0.35
fastSpeedMaxOutput = 0.5
---
maxOutputToEsc = 0.5
minOutputToEsc = 0.05

statusLedOn :: IO ()
statusLedOn = digitalWrite statusLedPin HIGH

statusLedOff :: IO ()
statusLedOff = digitalWrite statusLedPin LOW

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  inputsChan <- initInputsThread

  reactimate initialize (sense timeRef inputsChan) actuate outputsSignal

initialize :: IO Inputs
initialize = do
  putStrLn "Hello!"

  let handler = do
        pwmWrite pwmPin 0
        statusLedOff
        raiseSignal sigTERM
  installHandler keyboardSignal (Catch handler) Nothing

  putStrLn "Initializing PWM..."

  pinMode pwmPin PWM_OUTPUT

  pullUpDnControl statusLedPin PUD_UP
  pinMode statusLedPin OUTPUT

  pwmSetMode PWM_MODE_MS
  pwmSetClock pwmClock
  pwmSetRange pwmRange

  putStrLn "Initialized."

  return (defaultInputs)

initInputsThread :: IO (Chan (MaybeRawEvents))
initInputsThread = do
  inputsChan <- newChan

  let 
    loop = do
      statusLedOff
      maybeHandle <- try $ openFile "/dev/input/event0" ReadMode
      case maybeHandle of
        Right handle -> do
          traceIO "Connected to event interface."
          statusLedOn
          forever $ do
            maybeEvent <- try $ EvDev.hReadEvent handle
            case maybeEvent of
              Right (Just event) -> do
                writeChan inputsChan (RawInput event)
              Left e -> do
                return (isDoesNotExistError e) -- removes type ambig.
                writeChan inputsChan InputDisconnected
                loop
            threadDelay (1000 * 20)
            return ()
        Left e -> do
          return (isDoesNotExistError e) -- removes type ambig.
          writeChan inputsChan InputDisconnected
      threadDelay (1000 * 20)
      return ()
    in (forkIO . forever) $ loop

  return (inputsChan)

sense :: IORef UTCTime -> Chan (MaybeRawEvents) -> Bool -> IO (Double, Maybe Inputs)
sense timeRef inputsChan _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime

  maybeData <- timeout (1000 * 10) $ readChan inputsChan

  let 
    inputs = case maybeData of
      Nothing -> defaultInputs
      Just maybeRawEvents -> case maybeRawEvents of
                              InputDisconnected -> resetInputs
                              RawInput event -> interpretInput event
  
  return (realToFrac dt, Just inputs)


interpretInput :: EvDev.Event -> Inputs
interpretInput (EvDev.AbsEvent _ axis val) = newInputs {
  iJoystick =
    if axis == abs_hat0x
      then Event $ case compare val 0 of
                    LT -> 1.0
                    GT -> -1.0
                    EQ -> 0
      else NoEvent
}
  where
    abs_hat0x = EvDev.AbsAxis 16
    abs_hat0y = EvDev.AbsAxis 17
interpretInput (EvDev.KeyEvent _ key state) = newInputs {
    iBJoystick = check btn_joy,

    iBUp = check btn_up,
    iBDown = check btn_down,
    iBLeft = check btn_left,
    iBRight = check btn_right,

    iBTrigger = check btn_trigger,
    iBShoulder = check btn_shoulder
}
  where
    check x = if key == x 
                then case state of
                      EvDev.Depressed -> Event True
                      EvDev.Released -> Event False
                      _ -> NoEvent
                else NoEvent

    btn_joy = EvDev.Key 314
    btn_up = EvDev.Key 306
    btn_down = EvDev.Key 305
    btn_left = EvDev.Key 304
    btn_right = EvDev.Key 307
    btn_trigger = EvDev.Key 319
    btn_shoulder = EvDev.Key 318
interpretInput _ = defaultInputs

actuate :: Bool -> Outputs -> IO Bool
actuate _ outputs = do
  if isEvent $ oPrintBuffer outputs
    then do 
      putStrLn $ fromEvent $ oPrintBuffer outputs
      pwmWrite pwmPin $ oPWMOutput outputs
    else return ()
  return False

outputsSignal :: SF Inputs Outputs
outputsSignal = proc i -> do
  let userJoystickEvent = iJoystick i

  userConnected <- (arr $ not . isEvent) -< iDisconnected i
  userJoystickPosition <- hold 0.0 -< userJoystickEvent
  userJoystick <- hold False -< iBJoystick i
  userTrigger  <- hold False -< iBTrigger i
  userUp       <- hold False -< iBUp i
  userLeft     <- hold False -< iBLeft i
  userDown     <- hold False -< iBDown i

  let chosenMaxSpeed = if not userJoystick
                      then cruisingSpeedMaxOutput
                      else fastSpeedMaxOutput

  rampedThrottle <- rampedThrottleSF 0.0 -< userJoystickPosition
  clampedThrottle <- arr $ clamp (0.0, 1.0) -< rampedThrottle
  rescaledThrottle <- arr $ (0.0, 1.0) `rescale` (0.0, chosenMaxSpeed) -< clampedThrottle
  minFilteredThrottle <- arr $ (\throttle -> if throttle < minOutputToEsc
                                              then 0
                                              else throttle) -< rescaledThrottle
  calculatedThrottle <- identity -< minFilteredThrottle

  actualOutput <- rSwitch (constant 0.0) -< 
    (NoEvent, 
     fmap (\_ -> if userTrigger && userConnected
                  then if | userUp     -> constant 1.0
                          | userLeft   -> constant 0.5
                          | userDown   -> constant 0.0
                          | otherwise  -> arr $ (\_ -> calculatedThrottle)
                  else constant 0.0) (iNewUpdate i)
    )

  printMessageEvent <- repeatedly 0.3 () -< ()

  returnA -< Outputs {
    oPrintBuffer = printMessageEvent `tag` (show actualOutput),
    oPWMOutput = round $ (* (fromIntegral pwmRange)) $ (1.0 + actualOutput) / 20.0
  }
  where
    clamp (mn, mx) = max mn . min mx
    rescale (mn, mx) (mn', mx') = (*) ((mx' - mn') / (mx - mn))

    -- Throttle Signal Function constructors
    rampedThrottleSF initialThrottle = proc target -> do
      rec
        let error = target - position

        position <- integral -< (error * rampedThrottleP)
        --position <- integral -< (signum error * throttleStepPerSecond)

      returnA -< position

    