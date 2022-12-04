{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Global (
    Tick(..)
    , Name
    , height
    , width
) where

-- -- Constants

height, width :: Int
height = 20
width = 40

-- | Ticks mark passing of time

-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()