{-# OPTIONS_GHC -fplugin=Data.Functor.Identity.Plugin #-}

module Main where

import Data.Functor.Identity


data X f = X
  { foo :: f String
  }


myX :: X Identity
myX = X "world"


hello :: Show a => Identity a -> IO ()
hello = print


main :: IO ()
main = do
  hello "hello"
  putStrLn $ foo myX

