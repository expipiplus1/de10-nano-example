{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Cactus.Serial where

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment
import Cactus.SerialTX
import Cactus.SerialRX
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy

type Dom32 = Dom "CLK_32MHZ" 31250

topEntity
    :: Clock Dom32 Source
    -> Reset _ Asynchronous
    -> Signal _ Bit
    -> Signal _ Bit
topEntity = exposeClockReset board
  where
    board rxIn = txOut
      where
        TXOut{..} = tx serialDiv output'
        (output', fifoReady) = fifo (diff output) txReady

        -- input = regMaybe 0 $ rx serialDiv rxIn
        -- output = Just <$> input
        input = rx serialDiv rxIn
        output = gateMaybe <$> input <*> pure 0x41

serialDiv :: Word32
serialDiv = 3333

gateMaybe :: Maybe a -> b -> Maybe b
gateMaybe Nothing = const Nothing
gateMaybe (Just _) = Just
