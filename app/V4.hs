-- {-# OPTIONS_GHC -ddump-to-file -ddump-stg-final -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds -ddump-simpl #-}

{-# LANGUAGE TypeFamilies, MagicHash #-}

module V4 where

import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GM

data V4 = V4 !Float !Float !Float !Float

data instance UV.Vector    V4 =  V_V4 {-# UNPACK #-} !Int !(UV.Vector Float)
data instance UM.MVector s V4 = MV_V4 {-# UNPACK #-} !Int !(UM.MVector s Float)
instance UM.Unbox V4

instance GM.MVector UM.MVector V4 where
  basicLength (MV_V4 n _) = n
  basicUnsafeSlice m n (MV_V4 _ v) = MV_V4 n (GM.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_V4 _ v) (MV_V4 _ u) = GM.basicOverlaps v u
  basicUnsafeNew n = fmap (MV_V4 n) (GM.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_V4 _ v) i =
    do let o = 4*i
       x <- GM.basicUnsafeRead v o
       y <- GM.basicUnsafeRead v (o+1)
       z <- GM.basicUnsafeRead v (o+2)
       w <- GM.basicUnsafeRead v (o+3)
       return (V4 x y z w)
  basicUnsafeWrite (MV_V4 _ v) i (V4 x y z w) =
    do let o = 4*i
       GM.basicUnsafeWrite v o     x
       GM.basicUnsafeWrite v (o+1) y
       GM.basicUnsafeWrite v (o+2) z
       GM.basicUnsafeWrite v (o+3) w
  basicInitialize (MV_V4 _ v) = GM.basicInitialize v
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicInitialize #-}

instance GV.Vector UV.Vector V4 where
  basicUnsafeFreeze (MV_V4 n v) = fmap ( V_V4 n) (GV.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V4 n v) = fmap (MV_V4 n) (GV.basicUnsafeThaw   v)
  basicLength       ( V_V4 n _) = n
  basicUnsafeSlice m n (V_V4 _ v) = V_V4 n (GV.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_V4 _ v) i =
    do let o = 4*i
       x <- GV.basicUnsafeIndexM v o
       y <- GV.basicUnsafeIndexM v (o+1)
       z <- GV.basicUnsafeIndexM v (o+2)
       w <- GV.basicUnsafeIndexM v (o+3)
       return (V4 x y z w)
