{-# LANGUAGE RecordWildCards #-}
module InsertWith where

import Data.Vector.Hashtables.Internal
import Data.Vector.Hashtables.Internal.Mask
import Data.Hashable
import Data.Vector.Generic.Mutable
import Data.Bits
import Data.Primitive.MutVar
import Control.Monad
import Data.Primitive.PrimArray.Utils as A

insertWith
  :: ( MVector ks k, MVector vs v, DeleteEntry ks, DeleteEntry vs
     , PrimMonad m, Hashable k, Eq k
     )
  => Dictionary (PrimState m) ks k vs v -> (v -> v -> v) -> k -> v -> m ()
insertWith !ht f !k !v = do
  Dictionary{..} <- readMutVar . getDRef $ ht
  let
      hashCode' = hash k .&. mask
      targetBucket = hashCode' `rem` A.length buckets

      onFound dict i = do
        d'@Dictionary{..} <- readMutVar . getDRef $ dict
        v' <- value !~ i
        let !v'' = f v' v
        insertWithIndex targetBucket hashCode' k v'' (getDRef ht) d' i

      onNothing dict = do
        d' <- readMutVar . getDRef $ dict
        insertWithIndex targetBucket hashCode' k v (getDRef ht) d' (-1)

  void $ atWithOrElse ht k onFound onNothing
{-# INLINE insertWith #-}
