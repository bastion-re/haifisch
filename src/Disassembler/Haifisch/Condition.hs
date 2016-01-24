module Disassembler.Haifisch.Condition where

import Data.Binary
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import Data.Maybe
import Data.Tuple

import Disassembler.Haifisch.Binary

data Cond =
  -- ALU Conditions
  EQ | -- AZ = 1
  NE | -- AZ = 0
  GT | -- ALU > 0
  LT |
  GE |
  LE |
  AC | NOT_AC |
  AV | NOT_AV |
  -- Multiplier
  MV | NOT_MV |
  MS | NOT_MS |
  -- Shifter
  SV | NOT_SV |
  SZ | NOT_SZ |
  -- Bit Test
  TF | NOT_TF |
  -- Flag Input
  FLAG0 | NOT_FLAG0 |
  FLAG1 | NOT_FLAG1 |
  FLAG2 | NOT_FLAG2 |
  FLAG3 | NOT_FLAG3 |
  -- Mode
  BM | NOT_BM |
  -- Sequencer
  LCE |
  TRUE
  deriving (Show)

instance Enum Cond where
  fromEnum = fromJust $ flip lookup condLookupTable
  toEnum = fromJust $ flip lookup (fmap swap condLookupTable)

instance Binary Cond where
  get = runBitGet $ do
    b1 <- getBool
    b2 <- getBool
    b3 <- getBool
    b4 <- getBool
    b5 <- getBool
    let op = fromBits [b1, b2, b3, b4, b5]
    return $ fromEnum op
  put x = _putBits $ getBits $ toEnum x

condLookupTable :: [(Cond, Int)]
condLookupTable = [(EQ, 0), (LT, 1), (LE, 2), (AC, 3), (AV, 4),
                  (MV, 5), (MS, 6), (SV, 7), (SZ, 8), (FLAG0, 9),
                  (FLAG1, 10), (FLAG2, 11), (FLAG3, 12), (TF, 13),
                  (BM, 14), (LCE, 15), (NE, 16),
                  (GE, 17), (GT, 18), (NOT_AC, 19), (NOT_AV, 20),
                  (NOT_MV, 21), (NOT_MS, 22), (NOT_SV, 23),
                  (NOT_SZ, 24), (NOT_FLAG0, 25), (NOT_FLAG1, 26),
                  (NOT_FLAG2, 27), (NOT_FLAG3, 28), (NOT_TF, 29),
                  (NOT_BM, 30), (TRUE, 31)]
