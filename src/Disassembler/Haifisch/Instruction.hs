{-| -}
module Disassembler.Haifisch.Instruction where

import Disassembler.Haifisch.Register
import Disassembler.Haifisch.Computation
import Disassembler.Haifisch.Condition

-- | DMD, PMD
data AccessType = Read | Write

data Instruction =
  -- Group 1 Instructions - Computations
  -- Type 1 - Parallel data memory and program memory transfers with register file
  -- optional compute operation
  Type1 Compute |
  Type2 Cond Compute

