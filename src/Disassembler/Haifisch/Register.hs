module Disassembler.Haifisch.Register where

import Data.Tuple
import Data.Maybe
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Disassembler.Haifisch.Binary

data Register =
  -- Register File (ureg & dreg)
  -- Processing element X register file locations, fixed-point
  R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 |
  R14 | R15 |
  -- Processing element X register file locations, floating-point
  F0 | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 |
  F14 | F15 |
  -- Processing element Y register file locations, fixed-point
  S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 | S12 | S13 |
  S14 | S15 |
  -- Processing element Y register file locations, floating-point
  SF0 | SF1 | SF2 | SF3 | SF4 | SF5 | SF6 | SF7 | SF8 | SF9 | SF10 | SF11 |
  SF12 | SF13 | SF14 | SF15 |
  -- Program Sequencer
  PC | -- Program Counter
  PCSTK | -- Top of PC stack
  PCSTKP | -- PC stack pointer
  FADDR | -- Fetch address (read-only)
  DADDR | -- Decode address (read-only)
  LADDR | -- Loop termination address, code; top of loop address stack
  CURLCNTR | -- Current loop counter; top of loop count stack
  LCNTR | -- Loop count for next nested counter-controlled loop
  -- Data Address Generators
  I0 | I1 | I2 | I3 | I4 | I5 | I6 | I7 | -- DAG1 index registers
  M0 | M1 | M2 | M3 | M4 | M5 | M6 | M7 | -- DAG1 modify registers
  L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 | -- DAG1 length registers
  B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 | -- DAG1 base registers
  I8 | I9 | I10 | I11 | I12 | I13 | I14 | I15 | -- DAG2 index registers
  M8 | M9 | M10 | M11 | M12 | M13 | M14 | M15 | -- DAG2 modify registers
  L8 | L9 | L10 | L11 | L12 | L13 | L14 | L15 | -- DAG2 length registers
  B8 | B9 | B10 | B11 | B12 | B13 | B14 | B15 | -- DAG2 base registers
  -- Bus Exchange
  PX1 | PX2 | -- PMD-DMD bus exchange 1-2 (32 bits)
  PX | -- 64-bit combination of PX1 and PX2
  -- Timer
  TPERIOD | TCOUNT | -- Timer period, timer counter
  -- System registers (sreg & ureg)
  MODE1 | MODE2 | -- Mode control and status
  IRPTL | -- Interrupt latch
  IMASK | -- Interrupt mask
  IMASKP | -- Interrupt mask pointer (for nesting)
  MMASK | -- Mode mask
  FLAGS | -- Flag pins input/output state
  LIRPTL | -- Link port interrupt latch, mask, and pointer
  ASTATX | ASTATY | -- Element x/y arithmetic status flags, bit test flag, etc.
  STKYX | STKYY | -- Element x/y sticky arithmetic status flags, stack status flags, etc.
  USTAT1 | USTAT2 | USTAT3 | USTAT4 | -- User status register 1-4
  -- IOP Registers
  SYSCON | -- System control
  SYSTAT | -- System status
  WAIT | -- Memory wait states
  VIRPT | -- Multiprocessor IRQ
  MSGR0 | MSGR1 | MSGR2 | MSGR3 | MSGR4 | MSGR5 | MSGR6 | MSGR7 | -- Message registers
  BMAX | -- Bus timeout max
  BCNT | -- Bus timeout count
  ELAST | -- External address last
  -- IOP registers (DMA)
  EPB0 | EPB1 | EPB2 | EPB3 | -- External port FIFO buffers
  DMAC10 | DMAC11 | DMAC12 | DMAC13 | -- DMA controls (EPB0-3)
  DMASTAT | -- DMA status
  II0 | IM0 | C0 | CP0 | GP0 | DB0 | DA0 | -- DMA 0 parameters (SPORT0 RX)
  II1 | IM1 | C1 | CP1 | GP1 | DB1 | DA1 | -- DMA 1 parameters (SPORT1 RX)
  II2 | IM2 | C2 | CP2 | GP2 | DB2 | DA2 | -- DMA 2 parameters (SPORT0 TX)
  II3 | IM3 | C3 | CP3 | GP3 | DB3 | DA3 | -- DMA 3 parameters (SPORT1 TX)
  II4 | IM4 | C4 | CP4 | GP4 | DB4 | DA4 |
  II5 | IM5 | C5 | CP5 | GP5 | DB5 | DA5 |
  II6 | IM6 | C6 | CP6 | GP6 | DB6 | DA6 |
  II7 | IM7 | C7 | CP7 | GP7 | DB7 | DA7 |
  II8 | IM8 | C8 | CP8 | GP8 | DB8 | DA8 |
  II9 | IM9 | C9 | CP9 | GP9 | DB9 | DA9 |
  II10 | IM10 | C10 | CP10 | GP10 | EI10 | EM10 | EC10 |
  II11 | IM11 | C11 | CP11 | GP11 | EI11 | EM11 | EC11 |
  II12 | IM12 | C12 | CP12 | GP12 | EI12 | EM12 | EC12 |
  II13 | IM13 | C13 | CP13 | GP13 | EI13 | EM13 | EC13 |
  -- IOP registers (Link ports)
  LBUF0 | LBUF1 | LBUF2 | LBUF3 | LBUF4 | LBUF5 | -- Link port buffers
  LCTL0 | LCTL1 | -- Link buffer control
  LCOM | -- Link common control
  LAR | -- Link assignment
  LSRQ | -- Link service request
  LPATH1 | LPATH2 | LPATH3 | -- Link path (mesh)
  LPCNT | -- Link path count (mesh)
  CNST1 | CNST2 | -- Link constant (mesh)
  -- SPORT 0 registers
  STCTL0 | SRCTL0 | TX0 | RX0 | TDIV0 | RDIV0 | MTCS0 | MRCS0 |
  MTCCS0 | MRCCS0 | SPATH0 | KEYWD0 | KEYMASK0 |
  -- SPORT 1 registers
  STCTL1 | SRCTL1 | TX1 | RX1 | TDIV1 | RDIV1 | MTCS | MRCS1 |
  MTCCS1 | MRCCS1 | SPATH1 | KEYWD1 | KEYMASK1
  deriving (Show, Eq)

instance Enum Register where
  fromEnum = fromJust . flip lookup _SISDRegisterMap
  toEnum = fromJust . flip lookup (fmap swap _SISDRegisterMap)

data MRegister =
  MR | MR0 | MR1 | MR2 | -- Multiplier results
  MRF | MR0F | MR1F | MR2F | -- Multiplier results, foreground
  MRB | MR0B | MR1B | MR2B   -- Multiplier results, background
  deriving (Show, Eq)

-- | Processing Element X UREG codes
_SISDRegisterMap :: [(Register, Int)]
_SISDRegisterMap = [
                  (R0, 0), (R1, 1), (R2, 2), (R3, 3), (R4, 4), (R5, 5),
                  (R6, 6), (R7, 7), (R8, 8), (R9, 9), (R10, 10), (R11, 11),
                  (R12, 12), (R13, 13), (R14, 14), (R15, 15),
                  (I0, 16), (I1, 17), (I2, 18), (I3, 19), (I4, 20), (I5, 21),
                  (I6, 22), (I7, 23), (I8, 24), (I9, 25), (I10, 26), (I11, 27),
                  (I12, 28), (I13, 29), (I14, 30), (I15, 31),
                  (M0, 32), (M1, 33), (M2, 34), (M3, 35), (M4, 36), (M5, 37),
                  (M6, 38), (M7, 39), (M8, 40), (M9, 41), (M10, 42), (M11, 43),
                  (M12, 44), (M13, 45), (M14, 46), (M15, 47),
                  (L0, 48), (L1, 49), (L2, 50), (L3, 51), (L4, 52), (L5, 53),
                  (L6, 54), (L7, 55), (L8, 56), (L9, 57), (L10, 58), (L11, 59),
                  (L12, 60), (L13, 61), (L14, 62), (L15, 63),
                  (B0, 64), (B1, 65), (B2, 66), (B3, 67), (B4, 68), (B5, 69),
                  (B6, 70), (B7, 71), (B8, 72), (B9, 73), (B10, 74), (B11, 75),
                  (B12, 76), (B13, 77), (B14, 78), (B15, 79),
                  (S0, 80), (S1, 81), (S2, 82), (S3, 83), (S4, 84), (S5, 85),
                  (S6, 86), (S7, 87), (S8, 88), (S9, 89), (S10, 90), (S11, 91),
                  (S12, 92), (S13, 93), (S14, 94), (S15, 95),
                  (FADDR, 96), (DADDR, 97), (PC, 99), (PCSTK, 100), (PCSTKP, 101),
                  (LADDR, 102), (CURLCNTR, 103), (LCNTR, 104), {-(EMUCLK, 105),-}
                  {-(EMUCLK2, 106),-} (PX, 107), (PX1, 108), (PX2, 109),
                  (TPERIOD, 110), (TCOUNT, 111), (USTAT1, 112), (USTAT2, 113),
                  (MODE1, 114), (MMASK, 115), (MODE2, 116), (FLAGS, 117),
                  (ASTATX, 118), (ASTATY, 119), (STKYX, 120), (STKYY, 121),
                  (IRPTL, 122), (IMASK, 123), (IMASKP, 124), {-(LRPTL, 125),-}
                  (USTAT3, 126), (USTAT4, 127)
                  ]

-- | Processing Element Y (SIMD) UREG codes
_SIMDRegisterMap :: [(Register, Int)]
_SIMDRegisterMap = [
                  (S0, 0), (S1, 1), (S2, 2), (S3, 3), (S4, 4), (S5, 5), (S6, 6),
                  (S7, 7), (S8, 8), (S9, 9), (S10, 10), (S11, 11), (S12, 12),
                  (S13, 13), (S14, 14), (S15, 15),
                  (I0, 16), (I1, 17), (I2, 18), (I3, 19), (I4, 20), (I5, 21),
                  (I6, 22), (I7, 23), (I8, 24), (I9, 25), (I10, 26), (I11, 27),
                  (I12, 28), (I13, 29), (I14, 30), (I15, 31),
                  (M0, 32), (M1, 33), (M2, 34), (M3, 35), (M4, 36), (M5, 37),
                  (M6, 38), (M7, 39), (M8, 40), (M9, 41), (M10, 42), (M11, 43),
                  (M12, 44), (M13, 45), (M14, 46), (M15, 47),
                  (L0, 48), (L1, 49), (L2, 50), (L3, 51), (L4, 52), (L5, 53),
                  (L6, 54), (L7, 55), (L8, 56), (L9, 57), (L10, 58), (L11, 59),
                  (L12, 60), (L13, 61), (L14, 62), (L15, 63),
                  (B0, 64), (B1, 65), (B2, 66), (B3, 67), (B4, 68), (B5, 69),
                  (B6, 70), (B7, 71), (B8, 72), (B9, 73), (B10, 74), (B11, 75),
                  (B12, 76), (B13, 77), (B14, 78), (B15, 79),
                  (R0, 80), (R1, 81), (R2, 82), (R3, 83), (R4, 84), (R5, 85),
                  (R6, 86) , (R7, 87), (R8, 88), (R9, 89), (R10, 90), (R11, 91),
                  (R12, 92), (R13, 93), (R14, 94), (R15, 95),
                  (FADDR, 96), (DADDR, 97), (PC, 99), (PCSTK, 100), (PCSTKP, 101),
                  (LADDR, 102), (CURLCNTR, 103), (LCNTR, 104), {-(EMUCLK, 105),-}
                  {-(EMUCLK2, 106),-} (PX, 107), (PX2, 108), (PX1, 109),
                  (TPERIOD, 110), (TCOUNT, 111), (USTAT2, 112), (USTAT1, 113),
                  (MODE1, 114), (MMASK, 115), (MODE2, 116), (FLAGS, 117),
                  (ASTATY, 118), (ASTATX, 119), (STKYY, 120), (STKYX, 121),
                  (IRPTL, 122), (IMASK, 123), (IMASKP, 124), {-(LRPTL, 125),-}
                  (USTAT4, 126), (USTAT3, 127)]

-- | Read a 4-bit R-register code using the BitGet monad
-- | This will work for other non-R registers if they match the UREG table.
getRRegBits :: Data.Binary.Bits.Get.BitGet Register
getRRegBits = do
  r <- getWord8 4
  return $ toEnum $ fromIntegral r

-- | Put a 4-bit R-register code using the Put monad
-- | This will work for other non-R registers, but it won't be a 4-bit value.
putRRegBits :: Register -> Data.Binary.Bits.Put.BitPut ()
putRRegBits reg = putWord8 4 (fromIntegral $ fromEnum reg)

getFRegBits :: Data.Binary.Bits.Get.BitGet Register
getFRegBits = do
  r <- getWord8 4
  return $ case r of
    0 -> F0
    1 -> F1
    2 -> F2
    3 -> F3
    4 -> F4
    5 -> F5
    6 -> F6
    7 -> F7
    8 -> F8
    9 -> F9
    10 -> F10
    11 -> F11
    12 -> F12
    13 -> F13
    14 -> F14
    15 -> F15
    _ -> undefined

putFRegBits :: Register -> Data.Binary.Bits.Put.BitPut ()
putFRegBits F0 = putWord8 4 0x00
putFRegBits F1 = putWord8 4 0x01
putFRegBits F2 = putWord8 4 0x02
putFRegBits F3 = putWord8 4 0x03
putFRegBits F4 = putWord8 4 0x04
putFRegBits F5 = putWord8 4 0x05
putFRegBits F6 = putWord8 4 0x06
putFRegBits F7 = putWord8 4 0x07
putFRegBits F8 = putWord8 4 0x08
putFRegBits F9 = putWord8 4 0x09
putFRegBits F10 = putWord8 4 0x0a
putFRegBits F11 = putWord8 4 0x0b
putFRegBits F12 = putWord8 4 0x0c
putFRegBits F13 = putWord8 4 0x0d
putFRegBits F14 = putWord8 4 0x0e
putFRegBits F15 = putWord8 4 0x0f
putFRegBits _ = undefined

putMRegBits :: MRegister -> Data.Binary.Bits.Put.BitPut ()
putMRegBits = undefined
