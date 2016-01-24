module Disassembler.Haifisch.Binary where

import Data.Bits
import Data.Binary
import Data.Binary.Bits.Put (runBitPut, putBool)

-- | Return the value of the bit at the given position from the MSB.
getBit :: Int -> Int -> Bool
getBit = testBit

-- | Convert an int into a list of bools/bits from MSB to LSB
getBits :: Int -> [Bool]
getBits x  = fmap (getBit x) [0..n_bits] where
  n_bits = floor $ logBase 2 (fromIntegral x :: Double) + 1 :: Int

-- | Convert a list of bits from MSB to LSB to an Int
fromBits :: [Bool] -> Int
fromBits xs = sum $ zipWith (*) factors (fmap fromEnum (reverse xs)) where
  factors = fmap (2^) [0 .. len - 1]
  len = length xs

putBits :: [Int] -> Put
putBits bits = runBitPut $ do
  let _bits = fmap toEnum (reverse bits)
  mapM_ putBool _bits
  return ()

_putBits :: [Bool] -> Put
_putBits bits = runBitPut $ do
   let _bits = reverse bits
   mapM_ putBool _bits
   return ()
