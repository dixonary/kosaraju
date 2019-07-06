module Data.VASS.KarpMiller.ExtendedNaturals where

import Data.VASS
import Data.VASS.Shared
import Data.Functor
import Data.Vector (Vector)
import Data.List (intersperse)
import Data.Function ((&))
import Data.Coerce
import qualified Data.Vector as DV

-- * Extended configurations are those which include
-- the natural numbers, extended with the "unbounded" symbol Omega.

data ExtConf = ExtConf (Label State) ExtVector
    deriving (Eq)

type ExtVector = DV.Vector Nat


-- | Our ordering definition does NOT include the state.
-- To be fully correct, we should probably check the state is equal
-- as well; but we assume that this check only occurs when they are equal.

instance Ord ExtConf where
    ExtConf q a <= ExtConf q' b = a <= b && q == q'

instance Show ExtConf where
    show (ExtConf q c) = concat
        [ coerce q
        , ", <"
        , concat $ intersperse "," $ DV.toList $ fmap show c
        , ">"
        ]


-- | We use the natural partial ordering over vectors of natural numbers.
-- This is defined componentwise, ie a <= b iff every component in a is <= its
-- counterpart in b.
instance {-# OVERLAPS #-} Ord ExtVector where

    a <= b = DV.and $ DV.zipWith (<=) a b
    a >= b = DV.and $ DV.zipWith (>=) a b

    a <  b = DV.and (DV.zipWith (<=) a b) 
          && DV.or  (DV.zipWith (<)  a b)

    a >  b = DV.and (DV.zipWith (>=) a b) 
          && DV.or  (DV.zipWith (>)  a b)


-- | Extract a specific place from the (extended) configuration
getPlace :: Int -> ExtConf -> Nat
getPlace i (ExtConf _ vec) = vec DV.! i

-- | Convert from a (known finite) Nat to an Integer.
fromFinite :: Nat -> Integer
fromFinite (Finite x) = x

-- | The dimensionality of the configuration.
dim :: ExtConf -> Int
dim (ExtConf q v) = DV.length v

-- | Lift a non-omega configuration to an omega one.
extend :: Configuration -> ExtConf
extend (q,c) = ExtConf q (Finite <$> c)

activeFrom :: Transition -> ExtConf -> Bool
t `activeFrom` (ExtConf s n) = (Finite <$> pre t) <= n

-- | Apply some transition to a given configuration.
add :: Transition -> ExtConf -> ExtConf
add (pre, post, newState) (ExtConf _ conf) = ExtConf newState conf'
    where pre'  = Finite <$> pre
          post' = Finite <$> post
          conf' = conf
                    & flip (DV.zipWith (-)) pre'
                    &       DV.zipWith (+)  post'


-- * The natural number type
-- | Extended to allow for "unbounded" omega places.

data Nat     = Finite Integer | Omega
    deriving (Eq)

instance Num Nat where
    Finite x + Finite y = Finite (x + y)
    _        + _        = Omega

    Finite x - Finite y
        | y > x         = error "Integer dropped below zero!"
        | otherwise     = Finite (x - y)
    Omega    - Finite y = Omega
    _        - Omega    = error "Cannot subtract Omega"

    Finite x * Finite y = Finite (x * y)
    _        * _        = Omega

    abs           = id
    signum        = const 1
    fromInteger i = Finite (fromIntegral i)

instance Ord Nat where
    Finite x <= Finite y = x <= y
    _        <= Omega    = True
    Omega    <= Finite y = False


instance Show Nat where
    show (Finite i) = show i
    show Omega      = "ω"
