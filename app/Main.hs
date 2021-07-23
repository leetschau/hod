{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Notes
import System.Environment
import qualified Data.Text as T

main = do
  args <- getArgs
  parse $ map (\x -> T.pack x) args

