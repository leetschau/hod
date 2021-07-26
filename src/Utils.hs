{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Utils
    ( deQuote
    , deQuoteT
    ) where

import qualified Data.Text as T

deQuote :: String -> String
deQuote ('"': body) | last body == '"' = init body
deQuote ('\'': body) | last body == '\'' = init body
deQuote str = str

deQuoteT :: T.Text -> T.Text
deQuoteT str = T.pack $ deQuote $ T.unpack str

