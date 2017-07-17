{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( mktexfile
    ) where

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Yaml
import Control.Monad.Writer
import System.IO
import Data.List.Split (splitOn)
import Data.Foldable (sequenceA_)
import Numeric (showHex)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

type LaTeXString = Writer (DiffList String) ()

date :: IO (Integer, Int, Int)
date = getCurrentTime >>= return . toGregorian . utctDay

data PageNumber = PageNumber { num :: Integer 
                             , part :: Integer
                             }
instance FromJSON PageNumber where
  parseJSON (Object v) = PageNumber <$> v .: "page" <*> v .: "part"
instance ToJSON PageNumber where
  toJSON (PageNumber n p) = object [ "page" .= n
                                   , "part" .= p]

lastpagenum :: FilePath -> IO (Integer, Integer)
lastpagenum path = do
  info <- decodeFile path
  case info of
    Just (PageNumber n p) -> return (n,p)
    Nothing -> return . error $ "parse error"

mktexfile :: FilePath -> FilePath -> FilePath -> IO ()
mktexfile tempf conf outf = do
  (n,p) <- lastpagenum conf
  d <- date
  handle <- openFile tempf ReadMode
  templ <- hGetContents handle >>= (return . splitOn "\n")
  withFile outf WriteMode $ \h -> do
    mapM_ (hPutStrLn h) . fromDiffList . snd . runWriter $ 
      texfilestring n d p templ
  encodeFile conf $ PageNumber (n+16*p) p

putTexString :: String -> LaTeXString
putTexString s = tell . toDiffList $ [s]

texfilestring :: Integer -> (Integer, Int, Int) -> Integer -> [String] -> LaTeXString
texfilestring _ _ _ [] = putTexString []
texfilestring n (year,month,_) p xs = forM_ xs $ \x -> do
  case x of
    "%!INSERT_PAGE_COMMAND!" -> sequenceA_ $ pagelist +:+ sepcommand
    "%!INSERT_DATE_COMMAND!" -> putTexString $
      "\\chead{" ++ show year ++ "年  " ++ show month ++ "月}"
    _ -> putTexString x
    where
      pagelist = map pagecommand $ do
        part <- [0..(p-1)]
        halfpage <- map ((+) (16 * part)) [0..7]
        [halfpage, halfpage + 8]
      pagecommand np = do
        putTexString $ "\\"
        putTexString $ "\\lhead{" ++ showHex (n + np) "" ++ "}"
        putTexString $ "\\rhead{" ++ show (np `mod` 16) ++ "}"
      sepcommand    = putTexString $ "\\newpage"
      []     +:+ _ = []
      [x]    +:+ _ = [x]
      (x:xs) +:+ y = x:y:(xs +:+ y)
