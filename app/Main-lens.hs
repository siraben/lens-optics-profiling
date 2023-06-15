{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State

newtype Box3 = Box3
    { _n :: Int
    } deriving (Show)

newtype Box2 = Box2
    { _box3 :: Box3
    } deriving (Show)

newtype Box1 = Box1
    { _box2 :: Box2
    } deriving (Show)

makeLenses ''Box3
makeLenses ''Box2
makeLenses ''Box1

type BoxState = State Box1 ()

increment :: BoxState
increment = box2 . box3 . n %= (+1)

main :: IO ()
main = do
    let box = Box1 (Box2 (Box3 0))
    print (execState (replicateM_ 1000000 increment) box)

