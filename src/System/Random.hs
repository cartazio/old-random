{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This library deals with the common task of pseudo-random number
-- generation. The library makes it possible to generate repeatable
-- results, by starting with a specified initial random number generator,
-- or to get different results on each run by using the system-initialised
-- generator or by supplying a seed from some other source.
--
-- The library is split into two layers:
--
-- * A core /random number generator/ provides a supply of bits.
--   The class 'RandomGen' provides a common interface to such generators.
--   The library provides one instance of 'RandomGen', the abstract
--   data type 'StdGen'.  Programmers may, of course, supply their own
--   instances of 'RandomGen'.
--
-- * The class 'Random' provides a way to extract values of a particular
--   type from a random number generator.  For example, the 'Float'
--   instance of 'Random' allows one to generate random values of type
--   'Float'.
--
-- This implementation uses the Portable Combined Generator of L'Ecuyer
-- ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
-- Lennart Augustsson.  It has a period of roughly 2.30584e18.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module System.Random
        (

        -- $intro

        -- * Random number generators


          RandomGen(next, genRange)
        , SplittableGen(split)

        -- ** Standard random number generators
        --, StdGen
        --, mkStdGen

        -- ** The global random number generator

        -- $globalrng

        --, getStdRandom
        --, getStdGen
        --, setStdGen
        --, newStdGen

        -- * Random values of various types
        , Random ( randomDefault,   randomRange )
--,
                   --randoms,  randomRs,
                   --randomIO, randomRIO
        -- * References
        -- $references

        ) where

import Prelude

import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types



--import Data.Ratio       ( numerator, denominator )

--import Data.Char        ( isSpace, chr, ord )
--import System.IO.Unsafe ( unsafePerformIO )
--import Data.IORef       ( IORef, newIORef, readIORef, writeIORef )
#if MIN_VERSION_base (4,6,0)
--import Data.IORef       ( atomicModifyIORef' )
#else
import Data.IORef       ( atomicModifyIORef )
#endif
--import Numeric          ( readDec )

-- #ifdef __GLASGOW_HASKELL__
----import GHC.Exts         ( build )
-- #else
---- | A dummy variant of build without fusion.
--{-# INLINE build #-}
--build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
--build g = g (:) []
-- #endif

#if !MIN_VERSION_base (4,6,0)
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif



{- |

-}


-- | The class 'RandomGen' provides a common interface to random number
-- generators.
--
-- Minimal complete definition: 'next'.

class RandomGen g where

   -- |The 'next' operation returns an 'Int' that is uniformly distributed
   -- in the range returned by 'genRange' (including both end points),
   -- and a new generator.
   next     :: g -> (Word64, g)

   -- |The 'genRange' operation yields the range of values returned by
   -- the generator.
   --
   -- It is required that:
   --
   -- * If @(a,b) = 'genRange' g@, then @a < b@.
   --
   -- * 'genRange' always returns a pair of defined 'Word64's.
   --
   -- The second condition ensures that 'genRange' cannot examine its
   -- argument, and hence the value it returns can be determined only by the
   -- instance of 'RandomGen'.  That in turn allows an implementation to make
   -- a single call to 'genRange' to establish a generator's range, without
   -- being concerned that the generator returned by (say) 'next' might have
   -- a different range to the generator passed to 'next'.
   --
   -- The default definition spans the full range of 'Word64'.
   -- NB: any legal general purpose generator must be uniform over the range
   -- `0` through `2**64 -1`
   genRange :: g -> (Word64,Word64)

   -- default method
   genRange _ = (minBound, maxBound)


-- | The class 'SplittableGen' proivides a way to specify a random number
--   generator that can be split into two new generators.
class SplittableGen g where

   -- |The 'split' operation allows one to obtain two distinct random number
   -- generators. This is very useful in functional programs (for example, when
   -- passing a random number generator down to recursive calls), but very
   -- little work has been done on statistically robust implementations of
   -- 'split' (["System.Random\#Burton", "System.Random\#Hellekalek"]
   -- are the only examples we know of).
   split    :: g -> (g, g)


-- | The Random interval prior to 1.2 is implicitly a sampler typeclass for
-- data types that are naturally interpretable as intervals.
class Random a where
  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  randomRange :: RandomGen g => (a,a) -> g -> (a,g)

  -- | The same as 'randomRange', but using a default range determined by the type:
  --
  -- * For bounded enumerable types (instances of 'Bounded', such as 'Char'),
  --   the range is the closed interval [minBound,maxBound].
  -- * For 'Int' and 'Word', because their size is platform dependent, the default
  --   range is that of 'Int32' and 'Word32' for portability of sampling calculations
  --   across 32 and 64 bit systems.
  -- * Ctypes
  --
  -- * For floating point types, the range is the semi-closed interval
  --   @[0,1)@. The default implementation of 'Double' and 'Float' samplers will generate
  --   every representable normalized floating point value in this interval. It may or may not
  --   include denormalized floating point values. Faster samplers that sacrifice
  --   this guarantee are also provided in other modules.
  --
  -- * For 'Integer', the default range is the range of 'Int64'.
  -- * For 'Natural', the default range is the range of 'Word64'
  randomDefault  :: RandomGen g => g -> (a, g)


  -- | The interval bounds for values generated
  randomDefaultIntervalValue ::  p a -> (a,a)


-- | Plural variant of 'randomR', producing an infinite list of
-- random values instead of returning a new generator.
--{-# INLINE randomRs #-}
--randomRs :: (RandomGen g, Random a) => (a,a) -> g -> [a]
--randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

-- | Plural variant of 'random', producing an infinite list of
-- random values instead of returning a new generator.
--{-# INLINE randoms #-}
--randoms  :: (RandomGen g, Random a) => g -> [a]
--randoms  g      = build (\cons _nil -> buildRandoms cons random g)

-- | A variant of 'randomR' that uses the global random number generator
-- (see "System.Random#globalrng").
--randomRIO ::(Random a) => (a,a) -> IO a
--randomRIO range  =  error "implement"-- getStdRandom (randomR range)

-- | A variant of 'random' that uses the global random number generator
-- (see "System.Random#globalrng").
--randomIO  ::(Random a)=> IO a
--randomIO         = error "implement" --getStdRandom random


-- | Produce an infinite list-equivalent of random values.
--{-# INLINE buildRandoms #-}
--buildRandoms :: RandomGen g
--             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
--             -> (g -> (a,g))     -- ^ E.g. 'random'
--             -> g                -- ^ A 'RandomGen' instance
--             -> as
--buildRandoms cons rand = go
--  where
--    -- The seq fixes part of #4218 and also makes fused Core simpler.
--    go g = x `seq` (x `cons` go g') where (x,g') = rand g


instance Random Integer where
  randomRange ival g = randomIvalInteger ival g
  randomDefault g       = randomRange (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Int        where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Int8       where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Int16      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Int32      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Int64      where randomRange = randomIvalIntegral; randomDefault = randomBounded



instance Random Word       where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Word8      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Word16     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Word32     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random Word64     where randomRange = randomIvalIntegral; randomDefault = randomBounded

instance Random CChar      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CSChar     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CUChar     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CShort     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CUShort    where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CInt       where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CUInt      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CLong      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CULong     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CPtrdiff   where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CSize      where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CWchar     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CSigAtomic where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CLLong     where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CULLong    where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CIntPtr    where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CUIntPtr   where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CIntMax    where randomRange = randomIvalIntegral; randomDefault = randomBounded
instance Random CUIntMax   where randomRange = randomIvalIntegral; randomDefault = randomBounded

instance Random Char where
  --randomR (a,b) g =
  --     case (randomIvalInteger (toInteger (ord a), toInteger (ord b)) g) of
  --       (x,g') -> (chr x, g')
  --random g        = randomR (minBound,maxBound) g

instance Random Bool where
  randomRange (a,b) g =
      case (randomIvalInteger (bool2Int a, bool2Int b) g) of
        (x, g') -> (int2Bool x, g')
       where
         bool2Int :: Bool -> Integer
         bool2Int False = 0
         bool2Int True  = 1

         int2Bool :: Int -> Bool
         int2Bool 0     = False
         int2Bool _     = True

  randomDefault g        = randomRange (minBound,maxBound) g

{-# INLINE randomRFloating #-}
randomRFloating :: (Fractional a, Num a, Ord a, Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRFloating (l,h) g
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = randomDefault g in
                  (2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), g')  -- avoid overflow

instance Random Double where
  randomRange = randomRFloating
  randomDefault rng     =
    case randomDefault rng of
      (x,rng') ->
          -- We use 53 bits of randomness corresponding to the 53 bit significand:
          ((fromIntegral (mask53 .&. (x::Int64)) :: Double)
           /  fromIntegral twoto53, rng')
   where
    twoto53 = (2::Int64) ^ (53::Int64)
    mask53 = twoto53 - 1

instance Random Float where
  randomRange = randomRFloating
  randomDefault rng =
    -- TODO: Faster to just use 'next' IF it generates enough bits of randomness.
    case randomDefault rng of
      (x,rng') ->
          -- We use 24 bits of randomness corresponding to the 24 bit significand:
          ((fromIntegral (mask24 .&. (x::Int32)) :: Float)
           /  fromIntegral twoto24, rng')
         -- Note, encodeFloat is another option, but I'm not seeing slightly
         --  worse performance with the following [2011.06.25]:
--         (encodeFloat rand (-24), rng')
   where
     mask24 = twoto24 - 1
     twoto24 = (2::Int32) ^ (24::Int32)

-- CFloat/CDouble are basically the same as a Float/Double:
instance Random CFloat where
  randomRange = randomRFloating
  randomDefault rng = case randomDefault rng of
                 (x,rng') -> (realToFrac (x::Float), rng')

instance Random CDouble where
  --randomR = randomRFloating
  -- A MYSTERY:
  -- Presently, this is showing better performance than the Double instance:
  -- (And yet, if the Double instance uses randomFrac then its performance is much worse!)
  --random  = randomFrac
  -- random rng = case random rng of
  --             (x,rng') -> (realToFrac (x::Double), rng')



randomBounded :: (RandomGen g, Random a, Bounded a) => g -> (a, g)
randomBounded = randomRange (minBound, maxBound)

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

-- {-# SPECIALIZE randomIvalInteger :: (Num a) =>
--     (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q).  Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate random values until we exceed the target magnitude
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))


{--- The continuous functions on the other hand take an [inclusive,exclusive) range.
randomFrac :: (RandomGen g, Fractional a) => g -> (a, g)
randomFrac = randomIvalDouble (0::Double,1) realToFrac-}

--randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
--randomIvalDouble (l,h) fromDouble rng
--  | l > h     = randomIvalDouble (h,l) fromDouble rng
--  | otherwise =
--       case (randomIvalInteger (toInteger (minBound::Int32), toInteger (maxBound::Int32)) rng) of
--         (x, rng') ->
--            let
--             scaled_x =
--                fromDouble (0.5*l + 0.5*h) +                   -- previously (l+h)/2, overflowed
--                fromDouble ((0.5*h - 0.5*l) / (0.5 * realToFrac int32Count)) *  -- avoid overflow
--                fromIntegral (x::Int32)
--            in
--            (scaled_x, rng')


-- The global random number generator

{- $globalrng #globalrng#

There is a single, implicit, global random number generator of type
'StdGen', held in some global variable maintained by the 'IO' monad. It is
initialised automatically in some system-dependent fashion, for example, by
using the time of day, or Linux's kernel random number generator. To get
deterministic behaviour, use 'setStdGen'.
-}
{-
-- |Sets the global random number generator.
setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

-- |Gets the global random number generator.
getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ do
   rng <- mkStdRNG 0
   newIORef rng

-- |Applies 'split' to the current global random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef' theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = atomicModifyIORef' theStdGen (swap . f)
  where swap (v,g) = (g,v)
-}
