module Main where

import Notes
import System.Environment
import Data.List

main = do
  args <- getArgs
  parse args

