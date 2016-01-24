module Disassembler.Haifisch.Computation where

import Data.Binary hiding (getWord8, putWord8)
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import Data.Bits ((.&.), complement)
import Data.Maybe
import Data.Tuple

import Disassembler.Haifisch.Register
import Disassembler.Haifisch.Binary

data Mod1 = SSI | SUI | USI | UUI | SSF | SUF | USF | UUF |
            SSFR | SUFR | USFR | UUFR
            deriving (Show, Eq)

instance Enum Mod1 where
  fromEnum = fromJust . flip lookup mod1MaskTable
  toEnum = fromJust . flip lookup (fmap swap mod1MaskTable)

mod1MaskTable :: [(Mod1, Int)]
mod1MaskTable = [(SSI, 0x30), (SUI, 0x10), (USI, 0x20), (UUI, 0x00),
                (SSF, 0x38), (SUF, 0x18), (USF, 0x28), (UUF, 0x08),
                (SSFR, 0x39), (SUFR, 0x19), (USFR, 0x29),
                (UUFR, 0x09)]

data Mod2 = SI | UI | SF | UF
            deriving (Show, Eq)

instance Enum Mod2 where
  fromEnum = fromJust . flip lookup mod2MaskTable
  toEnum = fromJust . flip lookup (fmap swap mod2MaskTable)

mod2MaskTable :: [(Mod2, Int)]
mod2MaskTable = [(SI, 0x01), (UI, 0x00), (SF, 0x09), (UF, 0x08)]

data Mod3 = M3_SF | M3_UF
            deriving (Show, Eq)

instance Enum Mod3 where
  fromEnum M3_SF = 0x09
  fromEnum M3_UF = 0x08
  toEnum 0x08 = M3_UF
  toEnum 0x09 = M3_SF
  toEnum _ = undefined

data Compute =
  -- Fixed-point ALU Operations
  ADD Register Register Register | -- Rn = Rx + Ry
  SUB Register Register Register | -- Rn = Rx - Ry
  ADD_WITH_CARRY Register Register Register | -- Rn = Rx + Ry - CI
  SUB_WITH_BORROW Register Register Register | -- Rn = Rx - Ry + CI - 1
  ADD_DIV_2 Register Register Register | -- Rn = (Rx + Ry) / 2
  COMP Register Register | -- COMP(Rx, Ry)
  COMPU Register Register | -- COMPU(Rx, Ry)
  ADD_TO_CARRY Register Register | -- Rn = Rx + CI
  ADD_TO_BORROW Register Register | -- Rn = Rx + CI - 1
  INC Register Register | -- Rn = Rx + 1
  DEC Register Register | -- Rn = Rx - 1
  NEGATE Register Register | -- Rn = -Rx
  ABS Register Register | -- Rn = ABS(Rx)
  PASS Register Register | -- Rn = PASS(Rx)
  AND Register Register Register | -- Rn = Rx AND Ry
  OR Register Register Register | -- Rn = Rx OR Ry
  XOR Register Register Register | -- Rn = Rx XOR Ry
  NOT Register Register | -- Rn = NOT Rx
  MIN Register Register Register | -- Rn = MIN(Rx, Ry)
  MAX Register Register Register | -- Rn = MAX(Rx, Ry)
  CLIP Register Register Register | -- Rn = CLIP Rx BY Ry
  -- Floating-point ALU Operations
  ADD_F Register Register Register | -- Fn = Fx + Fy
  SUB_F Register Register Register | -- Fn = Fx - Fy
  ABS_SUM_F Register Register Register | -- Fn = ABS(Fx + Fy)
  ABS_DIFF_F Register Register Register | -- Fn = ABS(Fx - Fy)
  ADD_DIV_2_F Register Register Register | -- Fn = (Fx + Fy) / 2
  COMP_F Register Register | -- COMP(Fx, Fy)
  NEGATE_F Register Register | -- Fn = -Fx
  ABS_F Register Register | -- Fn = ABS(Fx)
  PASS_F Register Register | -- Fn = PASS(Fx)
  RND_F Register Register | -- Fn = RND(Fx)
  SCALB Register Register Register | -- Fn = SCALB Fx BY Ry
  MANT Register Register | -- Rn = MANT Fx
  LOGB Register Register | -- Rn = LOGB Fx
  FIX Register Register | -- Rn = FIX Fx
  FIX_BY Register Register Register | -- Rn = FIX Fx BY Ry
  FLOAT Register Register | -- Fn = FLOAT Rx
  FLOAT_BY Register Register Register | -- Fn = FLOAT Rx BY Ry
  RECIPS Register Register | -- Fn = RECIPS Fx
  RSQRTS Register Register | -- Fn = RSQRTS Fx
  COPYSIGN Register Register Register | -- Fn = Fx COPYSIGN Fy
  MIN_F Register Register Register | -- Fn = MIN(Fx, Fy)
  MAX_F Register Register Register | -- Fn = MAX(Fx, Fy)
  CLIP_F Register Register Register | -- Fn = CLIP Fx BY Fy
  -- Fixed-point multiplier operations
  MUL Register Register Register Mod1 | -- Rn = Rx * Ry mod2
  MUL_MR MRegister Register Register Mod1 | -- MRF/B = Rx * Ry
  MUL_ADDMR Register MRegister Register Register Mod1 | -- Rn = MRz + Rx * Ry mod2
  MUL_ADDMR_MR MRegister Register Register Mod1 | -- MRF/B = MRF/B + Rx * Ry
  MUL_SUBMR Register MRegister Register Register Mod1 | -- Rn = MRz - Rx * Ry mod2
  MUL_SUBMR_MR MRegister Register Register Mod1 | -- MRF/B = MRF/B - Rx * Ry
  SAT Register MRegister Mod2 | -- Rn = SAT MRF/MRB mod2
  SAT_MR MRegister MRegister Mod2 | -- MRF/B = SAT MRF/B
  RND Register MRegister Mod3 | -- Rn = RND MRF/MRB mod1
  RND_MR MRegister MRegister Mod3 | -- MRF/B = RND MRF/B
  ZERO_MR MRegister | -- MRF = 0 / MRB = 0
  MOV_MR_RN MRegister Register | -- MRxF/B = Rn
  MOV_RN_MR Register MRegister | -- Rn = MRxF/B
  -- Floating-point multiplier operations
  MUL_F Register Register Register | -- Fn = Fx * Fy
  -- Shifter operations
  LSHIFT Register Register Register | -- Rn = LSHIFT Rx BY Ry
  LSHIFT_IMM Register Register Word8 | -- Rn = LSHIFT Rx BY <data8>
  OR_LSHIFT Register Register Register | -- Rn = Rn OR LSHIFT Rx BY Ry
  OR_LSHIFT_IMM Register Register Word8 | -- Rn = Rn OR LSHIFT Rx BY <data8>
  ASHIFT Register Register Register | -- Rn = ASHIFT Rx BY Ry
  ASHIFT_IMM Register Register Word8 | -- Rn = ASHIFT Rx BY <data8>
  OR_ASHIFT Register Register Register | -- Rn = Rn OR ASHIFT Rx BY Ry
  OR_ASHIFT_IMM Register Register Word8 | -- Rn = Rn OR ASHIFT Rx BY <data8>
  ROT Register Register Register | -- Rn = ROT Rx BY Ry
  ROT_IMM Register Register Word8 | -- Rn = ROT Rx BY <data8>
  BCLR Register Register Register | -- Rn = BCLR Rx BY Ry
  BCLR_IMM Register Register Word8 | -- Rn = BCLR Rx BY <data8>
  BSET Register Register Register | -- Rn = BSET Rx BY Ry
  BSET_IMM Register Register Word8 | -- Rn = BSET Rx BY <data8>
  BTGL Register Register Register | -- Rn = BTGL Rx BY Ry
  BTGL_IMM Register Register Word8 | -- Rn = BTGL Rx BY <data8>
  BTST Register Register | -- BTST Rx BY Ry
  BTST_IMM Register Word8 | -- BTST Rx BY <data8>
  FDEP Register Register Register | -- Rn = FDEP Rx BY Ry
  FDEP_IMM Register Register Word8 Word8 | -- Rn = FDEP Rx BY <bit6>:<len6>
  OR_FDEP Register Register Register | -- Rn = Rn OR FDEP Rx BY Ry
  OR_FDEP_IMM Register Register Word8 Word8 | -- Rn = OR FDEP Rx BY <bit6>:<len6>
  FDEP_SE Register Register Register | -- Rn = FDEP Rx BY Ry (SE)
  FDEP_SE_IMM Register Register Word8 Word8 | -- Rn = FDEP Rx BY <bit6>:<len6> (SE)
  OR_FDEP_SE Register Register Register | -- Rn = Rn OR FDEP Rx BY Ry (SE)
  OR_FDEP_SE_IMM Register Register Word8 Word8 | -- Rn = Rn OR FDEP Rx BY <bit6>:<len6> (SE)
  FEXT Register Register Register | -- Rn = FEXT Rx BY Ry
  FEXT_IMM Register Register Word8 Word8 | -- Rn = FEXT Rx BY <bit6>:<len6>
  FEXT_SE Register Register Register | -- Rn = FEXT Rx BY Ry (SE)
  FEXT_SE_IMM Register Register Word8 Word8 | -- Rn = FEXT Rx BY <bit6>:<len6> (SE)
  EXP Register Register | -- Rn = EXP Rx
  EXP_EX Register Register | -- Rn = EXP Rx (EX)
  LEFTZ Register Register | -- Rn = LEFTZ Rx
  LEFTO Register Register | -- Rn = LEFTO Rx
  FPACK Register Register | -- Rn = FPACK Rx
  FUNPACK Register Register |  -- Fn = FUNPACK Rx
  NOP |
  Unknown (Word8, Word8, Word8)


instance Show Compute where
  show (ADD rn rx ry) = show rn ++ " = " ++ show rx ++ " + " ++ show ry
  show (SUB rn rx ry) = show rn ++ " = " ++ show rx ++ " - " ++ show ry
  show (ADD_WITH_CARRY rn rx ry) = show rn ++ " = " ++ show rx ++ " + " ++ show ry
                                   ++ " + CI"
  show (SUB_WITH_BORROW rn rx ry) = show rn ++ " = " ++ show rx ++ " - " ++ show ry
                                    ++ " + CI - 1"
  show (ADD_DIV_2 rn rx ry) = show rn ++ " = " ++ show rx ++ " + " ++ show ry ++ " / 2"
  show (COMP rx ry) = "COMP(" ++ show rx ++ ", " ++ show ry ++ ")"
  show (COMPU rx ry) = "COMPU(" ++ show rx ++ ", " ++ show ry ++ ")"
  show (ADD_TO_CARRY rn rx) = show rn ++ " = " ++ show rx ++ " + CI"
  show (ADD_TO_BORROW rn rx) = show rn ++ " = " ++ show rx ++ " + CI - 1"
  show (INC rn rx) = show rn ++ " = " ++ show rx ++ " + 1"
  show (DEC rn rx) = show rn ++ " = " ++ show rx ++ " - 1"
  show (NEGATE rn rx) = show rn ++ " = -" ++ show rx
  show (ABS rn rx) = show rn ++ " = ABS " ++ show rx
  show (PASS rn rx) = show rn ++ " = PASS " ++ show rx
  show (AND rn rx ry) = show rn ++ " = " ++ show rx ++ " AND " ++ show ry
  show (OR rn rx ry) = show rn ++ " = " ++ show rx ++ " OR " ++ show ry
  show (XOR rn rx ry) = show rn ++ " = " ++ show rx ++ " XOR " ++ show ry
  show (NOT rn rx) = show rn ++ " = NOT " ++ show rx
  show (MIN rn rx ry) = show rn ++ " = MIN(" ++ show rx ++ ", " ++ show ry ++ ")"
  show (MAX rn rx ry) = show rn ++ " = MAX(" ++ show rx ++ ", " ++ show ry ++ ")"
  show (CLIP rn rx ry) = show rn ++ " = CLIP " ++ show rx ++ " BY " ++ show ry
  show (ADD_F fn fx fy) = show fn ++ " = " ++ show fx ++ " + " ++ show fy
  show (SUB_F fn fx fy) = show fn ++ " = " ++ show fx ++ " - " ++ show fy
  show (ABS_SUM_F fn fx fy) = show fn ++ " = ABS(" ++ show fx ++ " + " ++ show fy ++ ")"
  show (ABS_DIFF_F fn fx fy) = show fn ++ " = ABS(" ++ show fx ++ " - " ++ show fy ++ ")"
  show (ADD_DIV_2_F fn fx fy) = show fn ++ " = (" ++ show fx ++ " + "
                                ++ show fy ++ ") / 2"
  show (COMP_F fx fy) = "COMP(" ++ show fx ++ ", " ++ show fy ++ ")"
  show (NEGATE_F fn fx) = show fn ++ " = -" ++ show fx
  show (ABS_F fn fx) = show fn ++ " = ABS " ++ show fx
  show (PASS_F fn fx) = show fn ++ " = PASS " ++ show fx
  show (RND_F fn fx) = show fn ++ " = RND " ++ show fx
  show (SCALB fn fx ry) = show fn ++ " = SCALB " ++ show fx ++ " BY " ++ show ry
  show (MANT rn fx) = show rn ++ " = MANT " ++ show fx
  show (LOGB rn fx) = show rn ++ " = LOGB " ++ show fx
  show (FIX rn fx) = show rn ++ " = FIX " ++ show fx
  show (FLOAT fn rx) = show fn ++ " = FLOAT " ++ show rx
  show (FLOAT_BY fn rx ry) = show fn ++ " = FLOAT " ++ show rx ++ " BY " ++ show ry
  show (RECIPS fn fx) = show fn ++ " = RECIPTS " ++ show fx
  show (RSQRTS fn fx) = show fn ++ " = RSQRTS " ++ show fx
  show (COPYSIGN fn fx fy) = show fn ++ " = " ++ show fx ++ " COPYSIGN " ++ show fy
  show (MIN_F fn fx fy) = show fn ++ " = MIN(" ++ show fx ++ ", " ++ show fy ++ ")"
  show (MAX_F fn fx fy) = show fn ++ " = MAX(" ++ show fx ++ ", " ++ show fy ++ ")"
  show (CLIP_F fn fx fy) = show fn ++ " = CLIP " ++ show fx ++ " BY " ++ show fy
  show (MUL rn rx ry mod) = show rn ++ " = " ++ show rx ++ " * " ++ show ry
                            ++ show " (" ++ show mod ++ show ")"
  show (MUL_ADDMR rn mr rx ry mod) = show rn ++ " = " ++ show mr ++ " + " ++ show rx
                                       ++ " * " ++ show ry ++ show " (" ++ show mod
                                       ++ ")"
  show (MUL_SUBMR rn mr rx ry mod) = show rn ++ " = " ++ show mr ++ " - " ++ show rx
                                       ++ " * " ++ show ry ++ " (" ++ show mod ++ ")"
  show (SAT rn mr mod) = show rn ++ " = SAT " ++ show mr ++ " (" ++ show mod ++ ")"
  show (RND rn mr mod) = show rn ++ " = RND " ++ show mr ++ " (" ++ show mod ++ ")"
  show (ZERO_MR mr) = show mr ++ " = 0"
  show (MOV_MR_RN mr rn) = show mr ++ " = " ++ show rn
  show (MOV_RN_MR rn mr) = show rn ++ " = " ++ show mr
  show (MUL_F fn fx fy) = show fn ++ " = " ++ show fx ++ " * " ++ show fy
  show (LSHIFT rn rx ry) = show rn ++ " = LSHIFT " ++ show rx ++ " BY " ++ show ry
  show (LSHIFT_IMM rn rx imm8) = show rn ++ " = LSHIFT " ++ show rx ++ " BY " ++ show imm8
  show (OR_LSHIFT rn rx ry) = show rn ++ " = " ++ show rn ++ " OR LSHIFT " ++ show rx
                              ++ " BY " ++ show ry
  show (OR_LSHIFT_IMM rn rx imm8) = show rn ++ " = " ++ show rn ++ " OR LSHIFT"
                                    ++ show rx ++ " BY " ++ show imm8
  show (ASHIFT rn rx ry) = show rn ++ " = ASHIFT " ++ show rx ++ " BY " ++ show ry
  show (ASHIFT_IMM rn rx imm8) = show rn ++ " = ASHIFT " ++ show rx ++ " BY " ++ show imm8
  show (OR_ASHIFT rn rx ry) = show rn ++ " = " ++ show rn ++ " OR ASHIFT"
                              ++ show rx ++ " BY " ++ show ry
  show (OR_ASHIFT_IMM rn rx imm8) = show rn ++ " = " ++ show rn ++ " OR ASHIFT "
                                    ++ show rx ++ " BY " ++ show imm8
  show (ROT rn rx ry) = show rn ++ " = ROT " ++ show rx ++ " BY " ++ show ry
  show (ROT_IMM rn rx imm8) = show rn ++ " = ROT " ++ show rx ++ " BY "
                              ++ show imm8
  show (BCLR rn rx ry) = show rn ++ " = BCLR " ++ show rx ++ " BY " ++ show ry
  show (BCLR_IMM rn rx imm8) = show rn ++ " = BCLR " ++ show rx ++ " BY " ++ show imm8
  show (BSET rn rx ry) = show rn ++ " = BSET " ++ show rx ++ " BY " ++ show ry
  show (BSET_IMM rn rx imm8) = show rn ++ " = BSET " ++ show rx ++ " BY " ++ show imm8
  show (BTGL rn rx ry) = show rn ++ " = BTGL " ++ show rx ++ " BY " ++ show ry
  show (BTGL_IMM rn rx imm8) = show rn ++ " = BTGL " ++ show rx ++ " BY " ++ show imm8
  show (BTST rx ry) = "BTST " ++ show rx ++ " BY " ++ show ry
  show (BTST_IMM rx imm8) = "BTST " ++ show rx ++ " BY " ++ show imm8
  show (FDEP rn rx ry) = show rn ++ " = FDEP " ++ show rx ++ " BY " ++ show ry
  show (FDEP_IMM rn rx bit6 len6) = show rn ++ " = FDEP " ++ show rx
                                    ++ " BY " ++ show bit6 ++ ":" ++ show len6
  show (OR_FDEP rn rx ry) = show rn ++ " = " ++ show rn ++ " OR FDEP " ++ show rx
                            ++ " BY " ++ show ry
  show (OR_FDEP_IMM rn rx bit6 len6) = show rn ++ " = " ++ show rn ++ " OR FDEP"
                                       ++ show rx ++ " BY " ++ show bit6 ++ ":" ++ show len6
  show (FDEP_SE rn rx ry) = show rn ++ " = FDEP " ++ show rx ++ " BY " ++ show ry ++ " (SE)"
  show (FDEP_SE_IMM rn rx bit6 len6) = show rn ++ " = FDEP " ++ show rx ++ " BY "
                                       ++ show bit6 ++ ":" ++ show len6 ++ " (SE)"
  show (OR_FDEP_SE rn rx ry) = show rn ++ " = " ++ show rn ++ " OR FDEP " ++ show rx
                               ++ " BY " ++ show ry ++ " (SE)"
  show (OR_FDEP_SE_IMM rn rx bit6 len6) = show rn ++ " = " ++ show rn ++ " OR FDEP " ++ show rx
                                          ++ " BY " ++ show bit6 ++ ":" ++ show len6 ++ " (SE)"
  show (FEXT rn rx ry) = show rn ++ " = FEXT " ++ show rx ++ " BY " ++ show ry
  show (FEXT_IMM rn rx bit6 len6) = show rn ++ " = FEXT " ++ show rx ++ " BY "
                                    ++ show bit6 ++ ":" ++ show len6
  show (FEXT_SE rn rx ry) = show rn ++ " = FEXT " ++ show rx ++ " BY " ++ show ry ++ " (SE)"
  show (FEXT_SE_IMM rn rx bit6 len6) = show rn ++ " = FEXT " ++ show rx ++ " BY "
                                       ++ show bit6 ++ ":" ++ show len6 ++ " (SE)"
  show (EXP rn rx) = show rn ++ " = EXP " ++ show rx
  show (EXP_EX rn rx) = show rn ++ " = EXP " ++ show rx ++ " (EX)"
  show (LEFTZ rn rx) = show rn ++ " = LEFTZ " ++ show rx
  show (LEFTO rn rx) = show rn ++ " = LEFTO " ++ show rx
  show (FPACK rn fx) = show rn ++ " = FPACK " ++ show fx
  show (FUNPACK fn rx) = show fn ++ " = FUNPACK " ++ show rx
  show NOP = "NOP"
  show (Unknown _) = "Unknown Instruction"

_put = joinPut . put

instance Binary Compute where
  get = undefined
  put (ADD rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (SUB rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_SUB
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (ADD_WITH_CARRY rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_WITH_CARRY
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (SUB_WITH_BORROW rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_SUB_WITH_BORROW
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (ADD_DIV_2 rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_DIV_2
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (COMP rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_COMP
    putWord8 4 0x00 >> putRRegBits rx >> putRRegBits ry
  put (COMPU rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_COMPU
    putWord8 4 0x00 >> putRRegBits rx >> putRRegBits ry
  put (ADD_TO_CARRY rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_TO_CARRY
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (ADD_TO_BORROW rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_TO_BORROW
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (INC rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_INC
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (DEC rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_DEC
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (NEGATE rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_NEGATE
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (ABS rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ABS
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (PASS rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_PASS
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (AND rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_AND
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (OR rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_OR
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (XOR rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_OR
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (NOT rn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_NOT
    putRRegBits rn >> putRRegBits rx >> putWord8 4 0x00
  put (MIN rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_MIN
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (MAX rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_MAX
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (CLIP rn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_CLIP
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (ADD_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (SUB_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_SUB_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (ABS_SUM_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ABS_SUM_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (ABS_DIFF_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ABS_DIFF_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (ADD_DIV_2_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ADD_DIV_2_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (COMP_F fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_COMP_F
    putWord8 4 0x00 >> putFRegBits fx >> putFRegBits fy
  put (NEGATE_F fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_NEGATE_F
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (ABS_F fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_ABS_F
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (PASS_F fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_PASS_F
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (RND_F fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_RND_F
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (SCALB fn fx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_SCALB
    putFRegBits fn >> putFRegBits fx >> putRRegBits ry
  put (MANT rn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_MANT
    putRRegBits rn >> putFRegBits fx >> putWord8 4 0x00
  put (LOGB rn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_LOGB
    putRRegBits rn >> putFRegBits fx >> putWord8 4 0x00
  put (FIX_BY rn fx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_FIX_BY
    putRRegBits rn >> putFRegBits fx >> putRRegBits ry
  put (FIX rn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_FIX
    putRRegBits rn >> putFRegBits fx >> putWord8 4 0x00
  put (FLOAT fn rx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_FLOAT
    putFRegBits fn >> putRRegBits rx >> putWord8 4 0x00
  put (FLOAT_BY fn rx ry) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_FLOAT_BY
    putFRegBits fn >> putRRegBits rx >> putRRegBits ry
  put (RECIPS fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_RECIPS
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (RSQRTS fn fx) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_RSQRTS
    putFRegBits fn >> putFRegBits fx >> putWord8 4 0x00
  put (COPYSIGN fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_COPYSIGN
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (MIN_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_MIN_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (MAX_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_MAX_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (CLIP_F fn fx fy) = runBitPut $ do
    putBool False >> _put ALU >> (joinPut . put) OP_CLIP_F
    putFRegBits fn >> putFRegBits fx >> putFRegBits fy
  put (MUL rn rx ry mod) = runBitPut $ do
    putBool True >> _put Mult >> (joinPut . put) OP_MUL
    putRRegBits rn >> putRRegBits rx >> putRRegBits ry
  put (MUL_MR MRF rx ry mod) = runBitPut $ do
    putBool True >> _put Mult >> (joinPut . put) (OP_MUL_MR MRF mod)


-- Opcodes
data ALUOpcode =
  -- Fixed-point ALU
  OP_ADD | OP_SUB | OP_ADD_WITH_CARRY | OP_SUB_WITH_BORROW | OP_ADD_DIV_2 |
  OP_COMP | OP_COMPU | OP_ADD_TO_CARRY | OP_ADD_TO_BORROW | OP_INC | OP_DEC |
  OP_NEGATE | OP_ABS | OP_PASS | OP_AND | OP_OR | OP_XOR | OP_NOT | OP_MIN |
  OP_MAX | OP_CLIP |
  -- Floating-point ALU
  OP_ADD_F | OP_SUB_F | OP_ABS_SUM_F | OP_ABS_DIFF_F |
  OP_ADD_DIV_2_F | OP_COMP_F | OP_NEGATE_F | OP_ABS_F | OP_PASS_F |
  OP_RND_F | OP_SCALB | OP_MANT | OP_LOGB | OP_FIX | OP_FIX_BY | OP_TRUNC_BY |
  OP_TRUNC | OP_FLOAT_BY | OP_FLOAT | OP_RECIPS | OP_RSQRTS | OP_COPYSIGN |
  OP_MIN_F | OP_MAX_F | OP_CLIP_F
  deriving (Show, Eq)

data MULOpcode =
  -- Multiplier
  -- Mult opcodes have 4 possible parameters encoded in them.
  -- y-input (1 = signed, 0 = unsigned)
  -- x-input (1 = signed, 0 = unsigned)
  -- format (1 = fractional, 0 = integer)
  -- rounding (1 = yes, 0 = no)
  OP_MUL Mod1 |
  OP_MUL_MR MRegister Mod1 |
  OP_MUL_ADDMR MRegister Mod1 |
  OP_MUL_ADDMR_MR MRegister Mod1 |
  OP_MUL_SUBMR MRegister Mod1 |
  OP_MUL_SUBMR_MR MRegister Mod1 |
  OP_SAT MRegister Mod2 | -- f x
  OP_SAT_MR MRegister Mod2 |
  OP_RND MRegister Mod3 | -- x
  OP_RND_MR MRegister Mod3 |
  OP_ZERO_MR MRegister |
  OP_MOV_MR |
  OP_MUL_F
  deriving (Show, Eq)

data SHIFTOpcode =
  -- Shifter Opcodes
  OP_LSHIFT | OP_OR_LSHIFT | OP_ASHIFT | OP_OR_ASHIFT | OP_ROT | OP_BCLR |
  OP_BSET | OP_BTGL | OP_BTST |
  -- FDEP/FEXT opcodes have a bool for SE (sign extend?) parameter
  OP_FDEP Bool | OP_OR_FDEP Bool | OP_FEXT Bool |
  OP_EXP Bool | -- EX bit here
  OP_LEFTZ | OP_LEFTO | OP_FPACK | OP_FUNPACK |
  OP_BITDEP |
  OP_BITEXT Bool | -- NU bit
  OP_BFFWRP_RN |
  OP_BFFWRP
  deriving (Show, Eq)

instance Enum ALUOpcode where
  fromEnum = fromJust . flip lookup _ALUOpcodeTable
  toEnum = fromJust . flip lookup (fmap swap _ALUOpcodeTable)

instance Binary ALUOpcode where
  get = runBitGet $ do
    word <- getWord8 8
    return $ toEnum (fromIntegral word)
  put op = runBitPut $ putWord8 8 (fromIntegral $ fromEnum op)

instance Binary MULOpcode where
  get = runBitGet $ do
    word <- getWord8 8 >>= fromIntegral
    let mod1AND = 0x39
    let mod2AND = 0x09
    let mod3AND = 0x01
    let mod1 = toEnum $ word (.&.) mod1AND
    let mod2 = toEnum $ word (.&.) mod2AND
    let mod3 = toEnum $ word (.&.) mod3AND
    let bits = fmap fromEnum (getBits word)

    case word (.&.) complement mod1AND of
      0x40 -> return OP_MUL mod1
      0x44 -> return OP_MUL_MR MRF mod1
      0x46 -> return OP_MUL_MR MRB mod1
      0x80 -> return OP_MUL_ADDMR MRF mod1
      0x82 -> return OP_MUL_ADDMR MRB mod1
      0x84 -> return OP_MUL_ADDMR_MR MRF mod1
      0x86 -> return OP_MUL_ADDMR_MR MRB mod1
      0xc0 -> return OP_MUL_SUBMR MRF mod1
      0xc2 -> return OP_MUL_SUBMR MRB mod1
      0xc4 -> return OP_MUL_SUBMR_MR MRF mod1
      0xc6 -> return OP_MUL_SUBMR_MR MRB mod1
    case word (.&.) complement mod2AND of
      0x00 -> return OP_SAT MRF mod2
      0x02 -> return OP_SAT MRB mod2
      0x04 -> return OP_SAT_MR MRF mod2
      0x06 -> return OP_SAT_MR MRB mod2
    case word (.&.) complement mod3AND of
      0x18 -> return OP_RND MRF mod3
      0x1a -> return OP_RND MRB mod3
      0x1c -> return OP_RND_MR MRF mod3
      0x1e -> return OP_RND_MR MRB mod3
    case word of
      0x14 -> return OP_ZERO_MR MRF
      0x16 -> return OP_ZERO_MR MRB
      0x00 -> return OP_MOV_MR
      0x30 -> return OP_MUL_F

  put (OP_MUL mod) = runBitPut $ putWord8 8 $ 0x40 (.&.) (fromEnum mod)
  put (OP_MUL_MR MRF mod) = runBitPut $ putWord8 8 $ 0x44 (.&.) (fromEnum mod)
  put (OP_MUL_MR MRB mod) = runBitPut $ putWord8 8 $ 0x46 (.&.) (fromEnum mod)
  put (OP_MUL_ADDMR MRF mod) = runBitPut $ putWord8 8 $ 0x80 (.&.) (fromEnum mod)
  put (OP_MUL_ADDMR MRB mod) = runBitPut $ putWord8 8 $ 0x82 (.&.) (fromEnum mod)
  put (OP_MUL_ADDMR_MR MRF mod) = runBitPut $ putWord8 8 $ 0x84 (.&.) (fromEnum mod)
  put (OP_MUL_ADDMR_MR MRB mod) = runBitPut $ putWord8 8 $ 0x86 (.&.) (fromEnum mod)
  put (OP_MUL_SUBMR MRF mod) = runBitPut $ putWord8 8 $ 0xc0 (.&.) (fromEnum mod)
  put (OP_MUL_SUBMR MRB mod) = runBitPut $ putWord8 8 $ 0xc2 (.&.) (fromEnum mod)
  put (OP_MUL_SUBMR_MR MRF mod) = runBitPut $ putWord8 8 $ 0xc4 (.&.) (fromEnum mod)
  put (OP_MUL_SUBMR_MR MRB mod) = runBitPut $ putWord8 8 $ 0xc6 (.&.) (fromEnum mod)
  put (OP_SAT MRF mod) = runBitPut $ putWord8 8 $ 0x00 (.&.) (fromEnum mod)
  put (OP_SAT MRB mod) = runBitPut $ putWord8 8 $ 0x02 (.&.) (fromEnum mod)
  put (OP_SAT_MR MRF mod) = runBitPut $ putWord8 8 $ 0x04 (.&.) (fromEnum mod)
  put (OP_SAT_MR MRB mod) = runBitPut $ putWord8 8 $ 0x06 (.&.) (fromEnum mod)
  put (OP_RND MRF mod) = runBitPut $ putWord8 8 $ 0x18 (.&.) (fromEnum mod)
  put (OP_RND MRB mod) = runBitPut $ putWord8 8 $ 0x1a (.&.) (fromEnum mod)
  put (OP_RND_MR MRF mod) = runBitPut $ putWord8 8 $ 0x1c (.&.) (fromEnum mod)
  put (OP_RND_MR MRB mod) = runBitPut $ putWord8 8 $ 0x1e (.&.) (fromEnum mod)
  put (OP_ZERO_MR MRF) = runBitPut $ putWord8 8 0x14
  put (OP_ZERO_MR MRB) = runBitPut $ putWord8 8 0x16
  put OP_MOV_MR = runBitPut $ putWord8 8 0x00
  put OP_MUL_F = runBitPut $ putWord8 8 0x30

instance Binary SHIFTOpcode where
  get = runBitGet $ do
    word <- getWord8 8
    return $ case word of
      0x00 -> OP_LSHIFT
      0x20 -> OP_OR_LSHIFT
      0x04 -> OP_ASHIFT
      0x24 -> OP_OR_ASHIFT
      0x08 -> OP_ROT
      0xc4 -> OP_BCLR
      0xc0 -> OP_BSET
      0xc8 -> OP_BTGL
      0xcc -> OP_BTST
      0x44 -> OP_FDEP False
      0x4c -> OP_FDEP True
      0x64 -> OP_OR_FDEP False
      0x6c -> OP_OR_FDEP True
      0x40 -> OP_FEXT False
      0x48 -> OP_FEXT True
      0x80 -> OP_EXP False
      0x84 -> OP_EXP True
      0x88 -> OP_LEFTZ
      0x8c -> OP_LEFTO
      0x90 -> OP_FPACK
      0x94 -> OP_FUNPACK
      0x74 -> OP_BITDEP
      0x50 -> OP_BITEXT False
      0x54 -> OP_BITEXT True
      0x7c -> OP_BFFWRP
      0x70 -> OP_BFFWRP_RN

  put OP_LSHIFT = runBitPut $ putWord8 8 0x00
  put OP_OR_LSHIFT = runBitPut $ putWord8 8 0x20
  put OP_ASHIFT = runBitPut $ putWord8 8 0x04
  put OP_OR_ASHIFT = runBitPut $ putWord8 8 0x24
  put OP_ROT = runBitPut $ putWord8 8 0x01
  put OP_BCLR = runBitPut $ putWord8 8 0xc4
  put OP_BSET = runBitPut $ putWord8 8 0xc0
  put OP_BTGL = runBitPut $ putWord8 8 0xc8
  put OP_BTST = runBitPut $ putWord8 8 0xcc
  put (OP_FDEP False) = runBitPut $ putWord8 8 0x44
  put (OP_FDEP True) = runBitPut $ putWord8 8 0x4c
  put (OP_OR_FDEP False) = runBitPut $ putWord8 8 0x64
  put (OP_OR_FDEP True) = runBitPut $ putWord8 8 0x6c
  put (OP_FEXT False) = runBitPut $ putWord8 8 0x60
  put (OP_FEXT True) = runBitPut $ putWord8 8 0x68
  put (OP_EXP False) = runBitPut $ putWord8 8 0x80
  put (OP_EXP True) = runBitPut $ putWord8 8 0x84
  put OP_LEFTZ = runBitPut $ putWord8 8 0x88
  put OP_LEFTO = runBitPut $ putWord8 8 0x8c
  put OP_FPACK = runBitPut $ putWord8 8 0x90
  put OP_FUNPACK = runBitPut $ putWord8 8 0x94
  put OP_BITDEP = runBitPut $ putWord8 8 0x74
  put (OP_BITEXT False) = runBitPut $ putWord8 8 0x50
  put (OP_BITEXT True) = runBitPut $ putWord8 8 0x58
  put OP_BFFWRP_RN = runBitPut $ putWord8 8 0x7c
  put OP_BFFWRP = runBitPut $ putWord8 8 0x70

_ALUOpcodeTable :: [(ALUOpcode, Int)]
_ALUOpcodeTable =
  [(OP_ADD, 0x01), (OP_SUB, 0x02), (OP_ADD_WITH_CARRY, 0x05),
   (OP_SUB_WITH_BORROW, 0x06), (OP_ADD_DIV_2, 0x09), (OP_COMP, 0x0a),
   (OP_COMPU, 0x0b), (OP_ADD_TO_CARRY, 0x25), (OP_ADD_TO_BORROW, 0x26),
   (OP_INC, 0x29), (OP_DEC, 0x2a), (OP_NEGATE, 0x22), (OP_ABS, 0x30),
   (OP_PASS, 0x21), (OP_AND, 0x40), (OP_OR, 0x41), (OP_XOR, 0x42),
   (OP_NOT, 0x43), (OP_MIN, 0x61), (OP_MAX, 0x62), (OP_CLIP, 0x63),
   (OP_ADD_F, 0x81), (OP_SUB_F, 0x82), (OP_ABS_SUM_F, 0x91),
   (OP_ABS_DIFF_F, 0x92), (OP_ADD_DIV_2_F, 0x89), (OP_COMP_F, 0x8a),
   (OP_NEGATE_F, 0xa2), (OP_ABS_F, 0xb0), (OP_PASS_F, 0xa1),
   (OP_RND_F, 0xa5), (OP_SCALB, 0xbd), (OP_MANT, 0xad), (OP_LOGB, 0xc1),
   (OP_FIX, 0xc9), (OP_FIX_BY, 0xd9), (OP_TRUNC_BY, 0xdd), (OP_TRUNC, 0xcd),
   (OP_FLOAT_BY, 0xda), (OP_FLOAT, 0xca), (OP_RECIPS, 0xc4), (OP_RSQRTS, 0xc5),
   (OP_COPYSIGN, 0xe0), (OP_MIN_F, 0xe1), (OP_MAX_F, 0xe2), (OP_CLIP_F, 0xe3)]

-- Compute Unit codes
data CU = ALU | Mult | Shift | UNDEF
        deriving (Show, Enum)

instance Binary CU where
  get = runBitGet $ do
    two <- getBool
    one <- getBool
    return $ case (two, one) of
      (False, False) -> ALU
      (False, True) -> Mult
      (True, False) -> Shift
      _ -> UNDEF
  put ALU = runBitPut $ putBool False >> putBool False
  put Mult = runBitPut $ putBool False >> putBool True
  put Shift = runBitPut $ putBool True >> putBool False
  put UNDEF = undefined

