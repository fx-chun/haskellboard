-- based on example in https://wiki.haskell.org/Yampa/reactimate

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import SDL.Input.Joystick

data Inputs = Inputs { 
  iThrottle       :: Throttle,

  iBUp            :: Event (),
  iBDown          :: Event (),
  iBLeft          :: Event (),
  iBRight         :: Event (),
  iBTrigger       :: Event (),
  iBShoulder      :: Event (),

  iDebug          :: Int
}

defaultInputs = Inputs {
  iThrottle = 0.0,

  iBUp = NoEvent,
  iBDown = NoEvent,
  iBLeft = NoEvent,
  iBRight = NoEvent,
  iBTrigger = NoEvent,
  iBShoulder = NoEvent,

  iDebug = 0
}

data Outputs = Outputs { 
  oPrintBuffer :: [Char],
  oPWMOutput :: DutyCycle
}

defaultOutputs = Outputs {
  oPrintBuffer = [],
  oPWMOutput = 0.0
}

type Throttle = Double
type DutyCycle = Double

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) actuate outputsSignal

initialize :: IO Inputs
initialize = do
  putStrLn "Hello... wait for it..."

  j <- numJoysticks

  return (defaultInputs {
    iDebug = fromIntegral j 
  })

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe Inputs)
sense timeRef _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime

  inputs <- gatherInputs

  return (realToFrac dt, Just inputs)

gatherInputs :: IO Inputs
gatherInputs = return (defaultInputs)

actuate :: Bool -> Outputs -> IO Bool
actuate _ outputs = do
  if null $ oPrintBuffer outputs
    then return ()
    else putStrLn $ oPrintBuffer outputs
  return False

outputsSignal :: SF Inputs Outputs
outputsSignal = arr (\i -> defaultOutputs {
  oPrintBuffer = show $ iDebug i
}) -- todo
