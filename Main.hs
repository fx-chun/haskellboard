{-# LANGUAGE Arrows, MultiWayIf #-}
-- based on example in https://wiki.haskell.org/Yampa/reactimate

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import FRP.Yampa.Geometry
import Control.Arrow
import Data.Map ((!))
import qualified Data.Map as Map

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

  iJoystick       :: Event JoystickState,
  iButton         :: Event ButtonState
} deriving (Show)

data JoystickState = Up
                   | Down
                   | Neutral deriving (Eq, Show)

data ButtonState = ButtonState Button Bool deriving (Eq, Show)

data Button = Trigger 
            | Shoulder
            | Joystick
            | DUp 
            | DDown 
            | DLeft 
            | DRight deriving (Eq, Show, Ord)

data MaybeRawEvents = InputDisconnected
                    | RawInput EvDev.Event

defaultInputs = Inputs {
  iNewUpdate = NoEvent,
  iDisconnected = NoEvent,

  iJoystick = NoEvent,
  iButton = NoEvent
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

---
pwmPin = Gpio 18
statusLedPin = Gpio 12
pwmClock = 94
pwmRange = 4096 :: PwmValue
---
rampedThrottleP = 0.70
--throttleStepPerSecond = 0.20
---
cruisingSpeedTarget = 0.32
fastSpeedTarget = 0.40
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
                    LT -> Up
                    GT -> Down 
                    EQ -> Neutral 
      else NoEvent
}
  where
    abs_hat0x = EvDev.AbsAxis 16
    abs_hat0y = EvDev.AbsAxis 17

interpretInput (EvDev.KeyEvent _ key state) = newInputs {
    iButton = buttonEvent
}
  where
    map' x y = if key == x 
                then case state of
                      EvDev.Depressed -> Event (ButtonState y True)
                      EvDev.Released  -> Event (ButtonState y False)
                      _ -> NoEvent
                else NoEvent

    buttonEvent = mergeEvents 
      [
        map' (EvDev.Key 319) Trigger,
        map' (EvDev.Key 318) Shoulder,
        map' (EvDev.Key 314) Joystick,
        map' (EvDev.Key 306) DUp,
        map' (EvDev.Key 305) DDown,
        map' (EvDev.Key 304) DLeft,
        map' (EvDev.Key 307) DRight
      ]

interpretInput _ = defaultInputs

actuate :: Bool -> Outputs -> IO Bool
actuate _ outputs = do
  if isEvent $ oPrintBuffer outputs
    then do
      putStrLn $ fromEvent $ oPrintBuffer outputs
      pwmWrite pwmPin $ oPWMOutput outputs
    else return ()
  return False

data ThrottleTargets = NoPower
                     | CruisingSpeed
                     | FastSpeed       deriving (Show)

outputsSignal :: SF Inputs Outputs
outputsSignal = proc i -> do
  isShoulderDown <- hold False -< isDown Shoulder i
  isTriggerDown  <- hold False -< isDown Trigger i
  isDUpDown      <- hold False -< isDown DUp i
  isDDownDown    <- hold False -< isDown DDown i
  isDLeftDown    <- hold False -< isDown DLeft i
  isDRightDown   <- hold False -< isDown DRight i

  let userInputMap = Map.fromList
        [
          (Shoulder, isShoulderDown),
          (Trigger, isTriggerDown),
          (DUp, isDUpDown),
          (DDown, isDDownDown),
          (DLeft, isDLeftDown),
          (DRight, isDRightDown)
        ]

  -- Normal Mode
  gas <-
    hold False 
    <<< (arr $ fmap (== Up))
    -< iJoystick i
 
  let speed = 
        if | userInputMap ! Shoulder -> FastSpeed
           | userInputMap ! Trigger  -> CruisingSpeed
           | otherwise               -> NoPower

  let normalTarget = 
        case speed of
          NoPower       -> 0.0
          CruisingSpeed -> cruisingSpeedTarget
          FastSpeed     -> fastSpeedTarget

  normalOutput <- rampedThrottleSF -< normalTarget

  -- Programming Mode

  programmingMode <-
    accumHoldBy (\acc _ -> not acc) False
    -< filterE (== (ButtonState DRight True)) $ iButton i

  let programmingOutput = 
        if | userInputMap ! DUp   -> 1.0
           | userInputMap ! DLeft -> 0.5
           | userInputMap ! DDown -> 0.0
           | otherwise            -> 0.0
  
  let output = 
        if | not programmingMode && gas -> normalOutput
           | programmingMode            -> programmingOutput
           | otherwise                  -> 0.0
      actualOutput = id
                     $ (0.0, 1.0) `rescale` (0.0, 0.7)
                     $ (\x -> if x < minOutputToEsc then 0.0 else x)
                     $ clamp (0, maxOutputToEsc)
                     $ output

  printMessageEvent <- repeatedly 0.3 () -< ()

  returnA -< Outputs {
    oPrintBuffer = printMessageEvent `tag` (show actualOutput),
    oPWMOutput = round $ (* (fromIntegral pwmRange)) 
                       $ (1.1 + actualOutput) / (20.0 :: Double)
  }
  where
    clamp (mn, mx) = max mn . min mx
    rescale (mn, mx) (mn', mx') = (*) ((mx' - mn') / (mx - mn))

    isDown button inputs = aux $ iButton inputs
      where
        aux (Event (ButtonState button x)) = Event x
        aux _ = NoEvent

    rampedThrottleSF = proc target -> do
      rec
        let error = target - position

        position <- integral -< (error * rampedThrottleP)
        --position <- integral -< (signum error * throttleStepPerSecond)

      returnA -< position