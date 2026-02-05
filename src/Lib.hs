{-# LANGUAGE OverloadedStrings #-}

-- | Library module with sample functions.
module Lib (
    add,
    greet,
) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- | Add two integers.
add :: Int -> Int -> Int
add a b = a + b

-- | Greet someone by name.
greet :: Text -> IO ()
greet name = TIO.putStrLn $ "Hello, " <> name <> "!"
