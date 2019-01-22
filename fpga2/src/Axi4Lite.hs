{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators  #-}

module Axi4Lite
  where

import           Clash.Prelude
-- import           Clash.Annotations.BitRepresentation
import           Data.Bool
import           Control.Monad
-- import           Clash.Annotations.BitRepresentation.Deriving
import           Data.Maybe

import Port

----------------------------------------------------------------
-- http://www.gstitt.ece.ufl.edu/courses/fall15/eel4720_5721/labs/refs/AXI4_specification.pdf
----------------------------------------------------------------

-- See table A3-3
data BurstType = BurstFixed | BurstIncr | BurstWrap
  deriving Generic
-- {-# ANN module (DataReprAnn
--                   $(liftQ [t|BurstType|])
--                   2
--                   [ ConstrRepr 'BurstFixed 0b11 0b00 []
--                   , ConstrRepr 'BurstIncr  0b11 0b01 []
--                   , ConstrRepr 'BurstWrap  0b11 0b10 []
--                   ]) #-}

-- See table A7-1
data AccessType = AccessNormal | AccessExclusive | AccessLocked
  deriving Generic
-- See table A3-4
-- {-# ANN module (DataReprAnn
--                   $(liftQ [t|AccessType|])
--                   2
--                   [ ConstrRepr 'AccessNormal    0b11 0b00 []
--                   , ConstrRepr 'AccessExclusive 0b11 0b01 []
--                   , ConstrRepr 'AccessLocked    0b11 0b10 []
--                   ]) #-}

-- See table A3-4
data Response = ResponseOK | ResponseExOK | ResponseSlErr | ResponseDecErr
  deriving (Generic)

instance BitPack Response where
  type BitSize Response = 2
  pack = \case
    ResponseOK     -> 0b00
    ResponseExOK   -> 0b01
    ResponseSlErr  -> 0b10
    ResponseDecErr -> 0b11
  unpack = \case
    0b00 -> ResponseOK
    0b01 -> ResponseExOK
    0b10 -> ResponseSlErr
    0b11 -> ResponseDecErr
    _    -> error "illegal unpack Response"

-- {-# ANN module (DataReprAnn
--                   $(liftQ [t|Response|])
--                   2
--                   [ ConstrRepr 'ResponseOK     0b11 0b00 []
--                   , ConstrRepr 'ResponseExOK   0b11 0b01 []
--                   , ConstrRepr 'ResponseSlErr  0b11 0b10 []
--                   , ConstrRepr 'ResponseDecErr 0b11 0b11 []
--                   ]) #-}

type Axi4LiteSlave addressWidth dataWidth f
  =  WriteAddressM addressWidth f
  -> WriteDataM dataWidth f
  -> WriteResponseM f
  -> ReadAddressM addressWidth f
  -> ReadDataM f
  -> ( WriteAddressS f
     , WriteDataS f
     , WriteResponseS f
     , ReadAddressS f
     , ReadDataS dataWidth f
     )

----------------------------------------------------------------
-- Writing
----------------------------------------------------------------

----------------------------------------------------------------
-- A2.2
-- Write address channel signals
--
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | Signal   | Source | Description                                                                              |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWID     | Master | Write address ID. This signal is the identification tag for the write address group      |
-- |          |        | of signals. See Transaction ID on page A5-77.                                            |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWADDR   | Master | Write address. The write address gives the address of the first transfer in a write      |
-- |          |        | burst transaction. See Address structure on page A3-44.                                  |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWLEN    | Master | Burst length. The burst length gives the exact number of transfers in a burst. This      |
-- |          |        | information determines the number of data transfers associated with the address.         |
-- |          |        | This changes between AXI3 and AXI4. See Burst length on page A3-44.                      |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWSIZE   | Master | Burst size. This signal indicates the size of each transfer in the burst. See Burst size |
-- |          |        | on page A3-45.                                                                           |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWBURST  | Master | Burst type. The burst type and the size information, determine how the address for       |
-- |          |        | each transfer within the burst is calculated. See Burst type on page A3-45.              |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWLOCK   | Master | Lock type. Provides additional information about the atomic characteristics of the       |
-- |          |        | transfer. This changes between AXI3 and AXI4.                                            |
-- |          |        | See Locked accesses on page A7-95.                                                       |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWCACHE  | Master | Memory type. This signal indicates how transactions are required to progress             |
-- |          |        | through a system. See Memory types on page A4-65.                                        |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWPROT   | Master | Protection type. This signal indicates the privilege and security level of the           |
-- |          |        | transaction, and whether the transaction is a data access or an instruction access.      |
-- |          |        | See Access permissions on page A4-71.                                                    |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWQOS    | Master | Quality of Service, QoS. The QoS identifier sent for each write transaction.             |
-- |          |        | Implemented only in AXI4. See QoS signaling on page A8-98.                               |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWREGION | Master | Region identifier. Permits a single physical interface on a slave to be used for         |
-- |          |        | multiple logical interfaces.                                                             |
-- |          |        | Implemented only in AXI4. See Multiple region signaling on page A8-99.                   |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWUSER   | Master | User signal. Optional User-defined signal in the write address channel.                  |
-- |          |        | Supported only in AXI4. See User-defined signaling on page A8-100.                       |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWVALID  | Master | Write address valid. This signal indicates that the channel is signaling valid write     |
-- |          |        | address and control information. See Channel handshake signals on page A3-38.            |
-- |----------|--------|------------------------------------------------------------------------------------------|
-- | AWREADY  | Slave  | Write address ready. This signal indicates that the slave is ready to accept an          |
-- |          |        | address and associated control signals. See Channel handshake signals on                 |
-- |          |        | page A3-38.                                                                              |
-- |----------|--------|------------------------------------------------------------------------------------------|

-- Write address signals from a master
data WriteAddressM addrWidth f = WriteAddressM
  { awaddr   :: f (BitVector addrWidth)
  , awprot   :: f (BitVector 3)
  , awvalid  :: f Bit
  }
  deriving (Generic)

deriving instance HasPortName (WriteAddressM addrWidth (Signal d))

newtype WriteAddressS f = WriteAddressS
  { awready  :: f Bit
  }
  deriving (Generic)

deriving instance HasPortName (WriteAddressS (Signal d))

-- Table A2-3, write data channel signals
--
-- | Signal | Source | Description                                                                                  |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WID    | Master | Write ID tag. This signal is the ID tag of the write data transfer. Supported only in AXI3.  |
-- |        |        | See Transaction ID on page A5-77.                                                            |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WDATA  | Master | Write data.                                                                                  |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WSTRB  | Master | Write strobes. This signal indicates which byte lanes hold valid data. There is one write    |
-- |        |        | strobe bit for each eight bits of the write data bus. See Write strobes on page A3-49.       |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WLAST  | Master | Write last. This signal indicates the last transfer in a write burst. See Write data channel |
-- |        |        | on page A3-39.                                                                               |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WUSER  | Master | User signal. Optional User-defined signal in the write data channel.                         |
-- |        |        | Supported only in AXI4. See User-defined signaling on page A8-100.                           |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WVALID | Master | Write valid. This signal indicates that valid write data and strobes are available. See      |
-- |        |        | Channel handshake signals on page A3-38.                                                     |
-- |--------|--------|----------------------------------------------------------------------------------------------|
-- | WREADY | Slave  | Write ready. This signal indicates that the slave can accept the write data. See Channel     |
-- |        |        | handshake signals on page A3-38.                                                             |

data WriteDataM dataWidth f = WriteDataM
  { wdata  :: f (BitVector dataWidth)
  , wstrb  :: f (BitVector (dataWidth `Div` 8))
  , wvalid :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (WriteDataM dataWidth (Signal d))

newtype WriteDataS f = WriteDataS
  { wready :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (WriteDataS (Signal d))

----------------------------------------------------------------
-- Write Response
----------------------------------------------------------------

-- Table A2-4
--
-- | Signal | Source | Description                                                                             |
-- |--------|--------|-----------------------------------------------------------------------------------------|
-- | BID    | Slave  | Response ID tag. This signal is the ID tag of the write response. See Transaction ID on |
-- |        |        | page A5-77.                                                                             |
-- | BRESP  | Slave  | Write response. This signal indicates the status of the write transaction. See Read and |
-- |        |        | write response structure on page A3-54.                                                 |
-- | BUSER  | Slave  | User signal. Optional User-defined signal in the write response channel. Supported only |
-- |        |        | in AXI4. See User-defined signaling on page A8-100.                                     |
-- | BVALID | Slave  | Write response valid. This signal indicates that the channel is signaling a valid write |
-- |        |        | response. See Channel handshake signals on page A3-38.                                  |
-- | BREADY | Master | Response ready. This signal indicates that the master can accept a write response. See  |
-- |        |        | Channel handshake signals on page A3-38.                                                |

newtype WriteResponseM f = WriteResponseM
  { bready :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (WriteResponseM (Signal d))

data WriteResponseS f = WriteResponseS
  { bresp :: f (BitVector (BitSize Response))
  , bvalid :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (WriteResponseS (Signal d))

----------------------------------------------------------------
-- Reading
----------------------------------------------------------------

-- Table A2-5
--
--  | Signal   | Source | Description                                                                                 |
--  |----------|--------|---------------------------------------------------------------------------------------------|
--  | ARID     | Master | Read address ID. This signal is the identification tag for the read address group of        |
--  |          |        | signals. See Transaction ID on page A5-77.                                                  |
--  | ARADDR   | Master | Read address. The read address gives the address of the first transfer in a read burst      |
--  |          |        | transaction. See Address structure on page A3-44.                                           |
--  | ARLEN    | Master | Burst length. This signal indicates the exact number of transfers in a burst. This          |
--  |          |        | changes between AXI3 and AXI4. See Burst length on page A3-44.                              |
--  | ARSIZE   | Master | Burst size. This signal indicates the size of each transfer in the burst. See Burst size on |
--  |          |        | page A3-45.                                                                                 |
--  | ARBURST  | Master | Burst type. The burst type and the size information determine how the address for each      |
--  |          |        | transfer within the burst is calculated. See Burst type on page A3-45.                      |
--  | ARLOCK   | Master | Lock type. This signal provides additional information about the atomic characteristics     |
--  |          |        | of the transfer. This changes between AXI3 and AXI4. See Locked accesses on                 |
--  |          |        | page A7-95.                                                                                 |
--  | ARCACHE  | Master | Memory type. This signal indicates how transactions are required to progress through        |
--  |          |        | a system. See Memory types on page A4-65.                                                   |
--  | ARPROT   | Master | Protection type. This signal indicates the privilege and security level of the transaction, |
--  |          |        | and whether the transaction is a data access or an instruction access. See Access           |
--  |          |        | permissions on page A4-71.                                                                  |
--  | ARQOS    | Master | Quality of Service, QoS. QoS identifier sent for each read transaction. Implemented         |
--  |          |        | only in AXI4. See QoS signaling on page A8-98.                                              |
--  | ARREGION | Master | Region identifier. Permits a single physical interface on a slave to be used for multiple   |
--  |          |        | logical interfaces. Implemented only in AXI4. See Multiple region signaling on              |
--  |          |        | page A8-99.                                                                                 |
--  | ARUSER   | Master | User signal. Optional User-defined signal in the read address channel.                      |
--  |          |        | Supported only in AXI4. See User-defined signaling on page A8-100.                          |
--  | ARVALID  | Master | Read address valid. This signal indicates that the channel is signaling valid read          |
--  |          |        | address and control information. See Channel handshake signals on page A3-38.               |
--  | ARREADY  | Slave  | Read address ready. This signal indicates that the slave is ready to accept an address      |
--  |          |        | and associated control signals. See Channel handshake signals on page A3-38.                |

data ReadAddressM addrWidth f = ReadAddressM
  { araddr :: f (BitVector addrWidth)
  , arprot :: f (BitVector 3)
  , arvalid :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (ReadAddressM addrWidth (Signal d))

newtype ReadAddressS f = ReadAddressS
  { arready :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (ReadAddressS (Signal d))

-- Table A2-6

--  | Signal | Source | Description                                                                                  |
--  |--------|--------|----------------------------------------------------------------------------------------------|
--  | RID    | Slave  | Read ID tag. This signal is the identification tag for the read data group of signals        |
--  |        |        | generated by the slave. See Transaction ID on page A5-77.                                    |
--  | RDATA  | Slave  | Read data.                                                                                   |
--  | RRESP  | Slave  | Read response. This signal indicates the status of the read transfer. See Read and write     |
--  |        |        | response structure on page A3-54.                                                            |
--  | RLAST  | Slave  | Read last. This signal indicates the last transfer in a read burst. See Read data channel on |
--  |        |        | page A3-39.                                                                                  |
--  | RUSER  | Slave  | User signal. Optional User-defined signal in the read data channel.                          |
--  |        |        | Supported only in AXI4. See User-defined signaling on page A8-100.                           |
--  | RVALID | Slave  | Read valid. This signal indicates that the channel is signaling the required read data. See  |
--  |        |        | Channel handshake signals on page A3-38.                                                     |
--  | RREADY | Master | Read ready. This signal indicates that the master can accept the read data and response      |
--  |        |        | information. See Channel handshake signals on page A3-38.                                    |

newtype ReadDataM f = ReadDataM
  { rready :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (ReadDataM (Signal d))

data ReadDataS dataWidth f = ReadDataS
  { rdata  :: f (BitVector dataWidth)
  , rresp  :: f (BitVector (BitSize Response))
  , rvalid :: f Bit
  }
  deriving(Generic)
deriving instance HasPortName (ReadDataS dataWidth (Signal d))

----------------------------------------------------------------
-- A simple slave
----------------------------------------------------------------


----------------------------------------------------------------
-- | Transaction channel    | Handshake pair   |
-- |------------------------|------------------|
-- | Write address channel  | AWVALID, AWREADY |
-- | Write data channel     | WVALID, WREADY   |
-- | Write response channel | BVALID, BREADY   |
-- | Read address channel   | ARVALID, ARREADY |
-- | Read data channel      | RVALID, RREADY   |
----------------------------------------------------------------

slave
  :: forall d gated synchronous addressWidth dataWidth
   . ( HiddenClockReset d gated synchronous
     , KnownNat addressWidth
     , KnownNat dataWidth
     )
  => Axi4LiteSlave addressWidth dataWidth (Signal d)
slave WriteAddressM {..} WriteDataM {..} WriteResponseM {..} ReadAddressM {..} ReadDataM {..}
  = ( WriteAddressS { .. }
    , WriteDataS { .. }
    , WriteResponseS { .. }
    , ReadAddressS { .. }
    , ReadDataS { .. }
    )
  where
    -- packing and unpacking
    writeAddressM =
      boolToMaybe (bitToBool <$> awvalid) ((,) <$> awaddr <*> awprot)
    writeDataM = boolToMaybe (bitToBool <$> wvalid) ((,) <$> wdata <*> wstrb)
    readAddressM =
      boolToMaybe (bitToBool <$> rvalid) ((,) <$> araddr <*> arprot)
    bresp  = pack . fromJust <$> writeResponseS
    bvalid = boolToBit . isJust <$> writeResponseS
    rdata  = fst . fromJust <$> readDataS
    rresp  = pack . snd . fromJust <$> readDataS
    rvalid = boolToBit . isJust <$> readDataS

    (firstWriteCommand, canPushWriteAddress, canPushWriteData) =
      unbundle $ fifoDualInput popWriteCommand
                               (guardA canPushWriteAddress writeAddressM)
                               (guardA canPushWriteData writeDataM)
    awready = boolToBit <$> canPushWriteAddress
    wready  = boolToBit <$> canPushWriteData

    -- We finish a writeCommand as soon as possible
    popWriteCommand =
      bool DontPop Pop
        <$> ((isJust <$> firstWriteCommand) .&&. canPushWriteResponseS)

    -- The slave must wait for wvalid and wready to be asserted before
    -- asserting bvalid
    --
    -- The slave must also wait until the last transaction
    writeResponseS :: Signal d (Maybe Response)
    (writeResponseS, canPushWriteResponseS) = fifo1
      (   bool DontPop Pop
      <$> ((bitToBool <$> bready) .&&. (isJust <$> writeResponseS))
      )
      (guardA
        ((== Pop) <$> popWriteCommand)
        (   fmap (\((_addr, _prot), (_data, _strb)) -> ResponseOK)
        <$> firstWriteCommand
        )
      )

    mem :: Signal d (BitVector dataWidth)
    mem =
      fromMaybe
        <$> register 0xabcdef99 mem
        <*> (fmap (fst . snd) <$> firstWriteCommand)

    ----------------------------------------------------------------
    -- reading
    ----------------------------------------------------------------

    (firstReadAddress, canPushReadAddress) =
      fifo1 popReadAddress (guardA canPushReadAddress readAddressM)
    arready = boolToBit <$> canPushReadAddress

    -- We consume a read address as soon as we can reply with the data
    popReadAddress =
      bool DontPop Pop <$> ((isJust <$> firstReadAddress) .&&. canPushReadDataS)

    readDataS :: Signal d (Maybe (BitVector dataWidth, Response))
    (readDataS, canPushReadDataS) = fifo1
      (bool DontPop Pop <$> ((bitToBool <$> rready) .&&. (isJust <$> readDataS))
      )
      (guardA
        ((== Pop) <$> popReadAddress)
        (   (\r m -> fmap
              (\(addr, _prot) -> (if addr == 0 then m else resize addr, ResponseOK)
              )
              r
            )
        <$> firstReadAddress
        <*> mem
        )
      )

data Pop = Pop | DontPop
  deriving(Eq)

-- A length 1 fifo with fallthrough
fifo1
  :: HiddenClockReset d gated synchronous
  => Signal d Pop
  -> Signal d (Maybe a)
  -> (Signal d (Maybe a), Signal d Bool)
  -- ^ (The first element in the fifo, can push)
fifo1 = curry (mealyB f Nothing)
  where
    f s (pop, push) =
      let
        -- We can push this cycle if we are popping, or if we have nothing
        -- waiting
        canPush = isNothing s || pop == Pop
        -- The thing at the front is what we have waiting, or what has just
        -- fallen through
        first   = s <|> push
        -- If we haven't popped then keep the existing state if there is any
        -- Or, if we aren't popping then what has been pushed in
        -- Or, if we are popping but were already storing something then what
        -- has been pushed in
        s' =
          (guard (pop == DontPop) >> s)
            <|> (guard (pop == DontPop || isJust s) >> push)
      in
        (s', (first, canPush))

-- |
--
-- >>> sampleN 3 $ fifoDualInput (fromList_lazy  [DontPop, DontPop, DontPop]) (fromList [Nothing, Just 1, Nothing]) (fromList [Nothing, Just "hello", Nothing])
-- [(Nothing,True,True),(Just (1,"hello"),True,True),(Just (1,"hello"),False,False)]
--
-- >>> sampleN 3 $ fifoDualInput (fromList_lazy  [DontPop, DontPop, DontPop]) (fromList [Nothing, Just 1, Nothing]) (fromList [Nothing, Nothing, Just "hello", Nothing])
-- [(Nothing,True,True),(Nothing,True,True),(Just (1,"hello"),False,True)]
fifoDualInput
  :: HiddenClockReset d gated synchronous
  => Signal d Pop
  -- ^ remove the first element from one or both fifos
  -> Signal d (Maybe a)
  -- ^ xs
  -> Signal d (Maybe b)
  -- ^ ys
  -> Signal d (Maybe (a, b), Bool, Bool)
  -- ^ (Aligned input, xs full, ys full)
fifoDualInput pop x y =
  let (xs, canPushXs) = fifo1 pop x
      (ys, canPushYs) = fifo1 pop y
  in  bundle (liftA2 (,) <$> xs <*> ys, canPushXs, canPushYs)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

boolToMaybe
  :: HiddenClockReset d gated synchronous
  => Signal d Bool
  -> Signal d a
  -> Signal d (Maybe a)
boolToMaybe = liftA2 (flip (bool Nothing . pure))

guardA
  :: HiddenClockReset d gated synchronous
  => Signal d Bool
  -> Signal d (Maybe a)
  -> Signal d (Maybe a)
guardA = liftA2 (flip (bool Nothing))

