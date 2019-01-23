{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module UART
  ( topEntity
  ) where

import Clash.Prelude
import Data.Proxy

import qualified Cactus.Serial as Serial

{-# ANN topEntity
  (Synthesize
    { t_name   = "uart_test"
    , t_inputs = [ PortName "clock_clk", PortName "reset_reset"
                 , PortName "loan_in"
                 ]
    , t_output = PortProduct ""
              [ PortName "loan_out"
              , PortName "loan_oe"
              , PortName "boot_from_fpga_on_failure"
              , PortName "boot_from_fpga_ready"
              , PortName "led_export"
              ]
    }
  )
  #-}
topEntity
  :: Clock Serial.Dom32 'Source
  -> Reset Serial.Dom32 'Asynchronous
  -> Signal Serial.Dom32 (BitVector 67)
  -> ( Signal Serial.Dom32 (BitVector 67)
     , Signal Serial.Dom32 (BitVector 67)
     , Signal Serial.Dom32 Bit
     , Signal Serial.Dom32 Bit
     , Signal Serial.Dom32 (BitVector 8)
     )
topEntity = exposeClockReset $ \loanIn ->
  let bootOnFailure = register 0 (pure 1)
      bootReady     = register 0 (pure 1)

      loanOE        = pure (replaceBit ledPin 1 . replaceBit txPin 1 $ 0)
      loanOut       = do
        t <- tx
        b <- blink
        pure (replaceBit ledPin b . replaceBit txPin t $ 0)

      hpsSwitch = (! switchPin) <$> loanIn

      rx        = (! rxPin) <$> loanIn
      tx        = hideClockReset Serial.topEntity rx

      leds      = mux (bitToBool <$> hpsSwitch) (pure 0xAA) ledCount
  in  ( loanOut :: Signal Serial.Dom32 (BitVector 67)
      , loanOE :: Signal Serial.Dom32 (BitVector 67)
      , bootOnFailure
      , bootReady
      , leds
      )

blink
  :: HiddenClockReset Serial.Dom32 gated synchronous => Signal Serial.Dom32 Bit
blink =
  let counter :: Signal Serial.Dom32 (Unsigned 25)
      counter = register 0 (satAdd SatWrap 1 <$> counter)
  in  msb <$> counter

ledCount
  :: HiddenClockReset Serial.Dom32 gated synchronous
  => Signal Serial.Dom32 (BitVector 8)
ledCount =
  let counter :: Signal Serial.Dom32 (Unsigned 25)
      counter = register 0 (satAdd SatWrap 1 <$> counter)
  in  slice d24 d17 <$> counter

rxPin = 49
txPin = 50

ledPin = 53
switchPin = 54
