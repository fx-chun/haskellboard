-- based on example in https://wiki.haskell.org/Yampa/reactimate

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa

data Inputs = Inputs { }

data Outputs = Outputs { }

outputs :: SF Inputs Outputs
outputs = arr (\_ -> Outputs {} ) -- todo

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) actuate outputs

initialize :: IO Inputs
initialize = do
  putStrLn "Hello... wait for it..."
  return (Inputs {})
 
actuate :: Bool -> Outputs -> IO Bool
actuate _ x = return False

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe Inputs)
sense timeRef _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime

  -- todo do input gathering here
  inputs <- undefined

  return (realToFrac dt, Just inputs)