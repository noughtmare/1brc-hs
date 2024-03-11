{-# OPTIONS_GHC -ddump-to-file -ddump-stg-final -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds -ddump-simpl #-}

{-# LANGUAGE TypeFamilies, MagicHash #-}

module Main (main {- , lazyParseMeasurements -}) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import FlatParse.Basic hiding (char, isDigit)
import Data.Char
import Control.Monad.IO.Class
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Hashtables
import V4
import InsertWith

type Hashtable k v = Dictionary (PrimState IO) VM.MVector k UM.MVector v

anyFloat :: ParserT st e Float
anyFloat = withOption (skipSatisfyAscii (== '-')) 
  (\_ -> do
    x <- anyAsciiDecimalInt <* skipSatisfyAscii (== '.') 
    c <- satisfyAscii isDigit
    pure $! negate $ fromIntegral x + fromIntegral (ord c - ord '0') * 0.1)
    -- anyPositiveFloat (\f -> pure $! negate f)) 
  (do
    x <- anyAsciiDecimalInt <* skipSatisfyAscii (== '.') 
    c <- satisfyAscii isDigit
    pure $! fromIntegral x + fromIntegral (ord c - ord '0') * 0.1)
{-# NOINLINE anyFloat #-}

--   (anyPositiveFloat (pure $!)) 
--   where
--   anyPositiveFloat k = do
--     x <- anyAsciiDecimalInt <* skipSatisfyAscii (== '.') 
--     c <- satisfyAscii isDigit
--     k $! fromIntegral x + fromIntegral (ord c - ord '0') * 0.1
--   {-# INLINE anyPositiveFloat #-}
-- {-# INLINE anyFloat #-}

parseLine :: Hashtable BS.ByteString V4 -> ParserIO () ()
parseLine !m = do
  name <- byteStringOf (skipSome (satisfyAscii (/= ';'))) <* skipAnyAsciiChar
  temp <- anyFloat <* skipAnyAsciiChar -- skipSatisfyAscii (== '\n')
  liftIO $ insertWith m f name $! V4 temp temp temp 1
  where
    f (V4 a b c d) (V4 a' b' c' d') = V4 (min a a') (max b b') (c+c') (d+d')
{-# NOINLINE parseLine #-}

parseMeasurements :: Hashtable BS.ByteString V4 -> ParserIO () ()
parseMeasurements !m = go where
  go = withOption (parseLine m) (const go) (pure ())
{-# NOINLINE parseMeasurements #-}

runParserLazy :: ParserIO e () -> B.ByteString -> IO ()
runParserLazy p bs0 = B.foldrChunks step (\_ -> pure ()) bs0 (OK () mempty) where
  step bs' go (OK () bs) = do
    _ <- runParserIO p (bs <> BS.snoc (BS.takeWhile (/= '\n') bs') '\n') 
    go =<< runParserIO p (BS.drop 1 (BS.dropWhile (/= '\n') bs'))
  step _ go x = go x

lazyParseMeasurements ht str = runParserLazy (parseMeasurements ht) str
{-# OPAQUE lazyParseMeasurements #-}

main :: IO ()
main = -- pure ()

  do
    str <- B.readFile "measurements-10m.txt"
    ht <- initialize 20000 :: IO (Hashtable BS.ByteString V4)
    _ <- runParserLazy (parseMeasurements ht) str
    ls <- toList ht
    mapM_ (\(k, V4 a b c d) -> print (k, a, b, c/d)) ls



-- 
-- -- {-# LANGUAGE MagicHash #-}
-- 
-- import qualified Data.HashMap.Strict as M
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy as B
-- import FlatParse.Basic hiding (char, isDigit)
-- import Data.Char
-- 
-- data T = T !Float !Float !Float !Float
-- data S = S {-# UNPACK #-} !BS.ByteString !Float
-- 
-- anyFloat :: Parser e Float
-- anyFloat = withOption (satisfyAscii (== '-')) (\_ -> anyPositiveFloat negate) (anyPositiveFloat id) where
--   anyPositiveFloat post = do
--     x <- anyAsciiDecimalInt <* skipSatisfyAscii (== '.') 
--     c <- satisfyAscii isDigit
--     pure $! post $ fromIntegral x + fromIntegral (ord c - ord '0') * 0.1
-- 
-- parseLine :: Parser e S
-- parseLine = do
--   name <- byteStringOf (skipSome (satisfyAscii (/= ';'))) <* skipAnyAsciiChar
--   temp <- anyFloat
--   skipSatisfyAscii (== '\n')
--   pure $! S name temp
-- 
-- parseMeasurements :: Parser e (M.HashMap BS.ByteString T)
-- parseMeasurements = go M.empty where
--   go !m = withOption parseLine (\(S k v) -> go (M.insertWith f k (T v v v 1) m)) (pure m)
-- 
-- f :: T -> T -> T
-- f (T a b c d) (T a' b' c' d') = T (min a a') (max b b') (c+c') (d+d')
-- 
-- runParserLazy :: (a -> a -> a) -> a -> Parser e a -> B.ByteString -> Result e a
-- runParserLazy k z p = B.foldlChunks step (OK z mempty) where
--   step (OK x bs) bs' = k x <$> runParser p (bs <> bs')
--   step x _ = x
-- 
-- main :: IO ()
-- main = do
--   str <- B.readFile "measurements.txt"
--   OK x _ <- pure $ runParserLazy (M.unionWith f) M.empty parseMeasurements str
--   M.foldrWithKey (\k (T a b c d) go -> print (k, a, b, c/d) *> go) (pure ()) x
