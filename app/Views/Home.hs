{-# LANGUAGE OverloadedStrings #-}

module Views.Home
  ( homeView
  ) where

import Lucid

homeView :: Html ()
homeView = do
  h1_ "Hello!"
  p_ "How are you today?"
  p_ "I've come quite a long way this weekend"
