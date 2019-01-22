{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Slave
  ( topEntity
  ) where

import           Clash.Prelude
import Data.Proxy

import Axi4Lite
import Port

{-# ANN topEntity
  (Synthesize
    { t_name   = "simple_axi3_slave"
    , t_inputs = [ PortName "clock_clk", PortName "reset_reset"
                 , getPortName (Proxy @(WriteAddressM 32 (Signal System))) "axs_s0_"
                 , getPortName (Proxy @(WriteDataM 32 (Signal System))) "axs_s0_"
                 , getPortName (Proxy @(WriteResponseM (Signal System))) "axs_s0_"
                 , getPortName (Proxy @(ReadAddressM 32 (Signal System))) "axs_s0_"
                 , getPortName (Proxy @(ReadDataM (Signal System))) "axs_s0_"
                 ]
    , t_output = PortProduct ""
              [ getPortName (Proxy @(WriteAddressS (Signal System))) "axs_s0_"
              , getPortName (Proxy @(WriteDataS (Signal System))) "axs_s0_"
              , getPortName (Proxy @(WriteResponseS (Signal System))) "axs_s0_"
              , getPortName (Proxy @(ReadAddressS (Signal System))) "axs_s0_"
              , getPortName (Proxy @(ReadDataS 32 (Signal System))) "axs_s0_"
              ]
    }
  )
  #-}
topEntity
  :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> Axi4LiteSlave AddressWidth DataWidth (Signal System)
topEntity = exposeClockReset slave

type AddressWidth = 16
type DataWidth = 32
