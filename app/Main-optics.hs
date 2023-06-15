{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import GHC.Generics
import Optics
import Optics.State.Operators
import Control.Monad.State

newtype Box3 = Box3
    { n :: Int
    } deriving (Generic, Show)

newtype Box2 = Box2
    { box3 :: Box3
    } deriving (Generic, Show)

newtype Box1 = Box1
    { box2 :: Box2
    } deriving (Generic, Show)

makeFieldLabels ''Box3
makeFieldLabels ''Box2
makeFieldLabels ''Box1

type BoxState = State Box1 ()

increment :: BoxState
increment = #box2 % #box3 % #n %= (+ 1)

main :: IO ()
main = do
    let box = Box1 (Box2 (Box3 0))
    print (execState (replicateM_ 1000000 increment) box)

