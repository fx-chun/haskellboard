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
import qualified System.Linux.Input.Event as EvDev

import Debug.Trace

data Inputs = Inputs { 
  iThrottle       :: Event Throttle,

  iBUp            :: Event (),
  iBDown          :: Event (),
  iBLeft          :: Event (),
  iBRight         :: Event (),
  iBTrigger       :: Event (),
  iBShoulder      :: Event (),

  iDebug          :: Int
}

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
}

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
                return (isDoesNotExistError e) -- removes type ambig.
                writeChan inputsChan Nothing
                loop
              Right event -> do
                writeChan inputsChan event
          return ()
    in (forkIO . forever) $ loop

  return (inputsChan)

sense :: IORef UTCTime -> Chan (Maybe RawInputs) -> Bool -> IO (Double, Maybe Inputs)
sense timeRef inputsChan _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime

  rawInputs <- readChan inputsChan
  let 
    inputs = case rawInputs of
      Nothing -> defaultInputs
      Just x -> interpretInput x
  
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

interpretInput _ = defaultInputs

actuate :: Bool -> Outputs -> IO Bool
actuate _ outputs = do
  if isEvent $ oPrintBuffer outputs
    then putStrLn $ fromEvent $ oPrintBuffer outputs
    else return ()
  return False

outputsSignal :: SF Inputs Outputs
outputsSignal = proc i -> do
  rec
    let throttle = iThrottle i

  returnA -< Outputs {
    oPrintBuffer = (iThrottle i) `tag` (show throttle)
  }
