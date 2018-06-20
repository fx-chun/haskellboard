{-# LANGUAGE Arrows #-}
-- based on example in https://wiki.haskell.org/Yampa/reactimate

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import FRP.Yampa.Geometry

import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import System.Linux.Input.Device
import System.Timeout
import qualified System.Linux.Input.Event as EvDev

import Debug.Trace

data Inputs = Inputs { 
  iThrottle       :: Event Throttle,

  iBUp            :: Event Bool,
  iBDown          :: Event Bool,
  iBLeft          :: Event Bool,
  iBRight         :: Event Bool,
  iBTrigger       :: Event Bool,
  iBShoulder      :: Event Bool,

  iDebug          :: Int
} deriving (Show)

type RawInputs = EvDev.Event

defaultInputs = Inputs {
  iThrottle = NoEvent,

  iBUp = NoEvent,
  iBDown = NoEvent,
  iBLeft = NoEvent,
  iBRight = NoEvent,
  iBTrigger = NoEvent,
  iBShoulder = NoEvent,

  iDebug = 0
}

data Outputs = Outputs { 
  oPrintBuffer :: Event [Char],
  oPWMOutput :: DutyCycle
} deriving (Show)

defaultOutputs = Outputs {
  oPrintBuffer = NoEvent,
  oPWMOutput = 0.0
} 

type Throttle = Double
type DutyCycle = Double

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t

  inputsChan <- initInputsThread

  reactimate initialize (sense timeRef inputsChan) actuate outputsSignal

initialize :: IO Inputs
initialize = do
  putStrLn "Hello... wait for it..."

  return (defaultInputs)

initInputsThread :: IO (Chan (Maybe RawInputs))
initInputsThread = do
  inputsChan <- newChan

  let 
    loop = do
      maybeHandle <- try $ openFile "/dev/input/event0" ReadMode
      case maybeHandle of
        Left e -> do
          return (isDoesNotExistError e) -- removes type ambig.
          writeChan inputsChan Nothing
        Right handle -> do
          traceIO "Connected to event interface."
          forever $ do
            maybeEvent <- try $ EvDev.hReadEvent handle
            case maybeEvent of
              Left e -> do
                traceIO "Disconnected from event interface."
                return (isDoesNotExistError e) -- removes type ambig.
                writeChan inputsChan Nothing
                loop
              Right event -> do
                writeChan inputsChan event
            threadDelay (1000 * 10)
            return ()
      threadDelay (1000 * 10)
      return ()
    in (forkIO . forever) $ loop

  return (inputsChan)

sense :: IORef UTCTime -> Chan (Maybe RawInputs) -> Bool -> IO (Double, Maybe Inputs)
sense timeRef inputsChan _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime

  maybeData <- timeout (1000 * 5) $ readChan inputsChan

  let 
    inputs = case maybeData of
      Nothing -> defaultInputs
      Just maybeRawInputs -> case maybeRawInputs of
                              Nothing -> defaultInputs
                              Just rawInputs -> interpretInput rawInputs
  
  return (realToFrac dt, Just inputs)


interpretInput :: RawInputs -> Inputs
interpretInput (EvDev.AbsEvent _ axis val) = defaultInputs {
  iThrottle =
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
interpretInput (EvDev.KeyEvent _ key state) = defaultInputs {
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
    then putStrLn $ fromEvent $ oPrintBuffer outputs
    else return ()
  return False

outputsSignal :: SF Inputs Outputs
outputsSignal = proc i -> do
  let throttleEvent = iThrottle i
      shoulderEvent = iBShoulder i

  throttle <- rSwitch smoothThrottleSF -< (throttleEvent, 
                                           fmap (\x -> if x 
                                                        then rawThrottleSF 
                                                        else smoothThrottleSF) shoulderEvent)

  clampedThrottle <- (arr $ clamp (-1.0) 1.0) -< throttle

  returnA -< Outputs {
    oPrintBuffer = Event (show throttle),
    oPWMOutput = (((throttle * 0.5) + 1.0) / 2.0) * 1024.0
  }
  where
    clamp mn mx = max mn . min mx

    -- SF (Event Throttle) Throttle
    smoothThrottleSF = proc targetUpdate -> do
      rec
        target <- hold 0.0 -< targetUpdate
        let error = target - position
        position <- integral -< (error * 0.25)

      returnA -< position
      
    
    rawThrottleSF = hold 0.0
