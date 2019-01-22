{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Blink
  ( topEntity
  ) where

import           Clash.Prelude

{-# ANN topEntity
  (Synthesize
    { t_name   = "Blink"
    , t_inputs = [PortName "clk", PortName "reset"]
    , t_output = PortName "blink"
    }
  )
  #-}
topEntity
  :: Clock System 'Source -> Reset System 'Asynchronous -> Signal System Bit
topEntity = exposeClockReset $ count (== 0) counter

count
  :: (Undefined b, Enum b, Bounded b, HiddenClockReset domain gates synchronous)
  => (a -> Bool)
  -> Signal domain a
  -> Signal domain b
count p s = let r = register minBound (mux (p <$> s) (succ <$> r) r) in r

counter
  :: HiddenClockReset domain gates synchronous
  => Signal domain (Index 50000000)
counter = s
  where
    s        = register 0 (wrapSucc <$> s)
    wrapSucc = satAdd SatWrap 1
