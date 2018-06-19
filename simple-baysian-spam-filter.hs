-- | An implementation of  the spam filter from Paul Graham's A Plan for Spam.
module Spam where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid

newtype Token = Token String
  deriving (Ord,Eq,Show)

data Corpus =
  Corpus {corpusMessages :: Double
         ,corpusHistogram :: Map Token Double}
  deriving (Show)

instance Monoid Corpus where
  mempty = Corpus 0 mempty
  mappend (Corpus a x) (Corpus b y) =
    Corpus (a + b) (M.unionWith (+) x y)

corpus :: [String] -> Corpus
corpus = foldl' (<>) mempty . map (Corpus 1 . histogram . tokenize)

tokenize :: String -> [Token]
tokenize = map Token . words

histogram :: [Token] -> Map Token Double
histogram = foldl' (\m t -> M.insertWith (+) t 1 m) mempty

occurances :: Double
occurances = 1 -- Turn this up to 5 when the corpus gets bigger.

probability :: Corpus -> Corpus -> Token -> Maybe Double
probability bad good token =
  if g + b < occurances
     then Nothing
     else Just
            (max 0.01
                 (min 0.99 ((min 1 (b / nbad)) /
                            (min 1 (g / ngood) + (min 1 (b / nbad))))))
  where g = 2 * M.findWithDefault 0 token (corpusHistogram good)
        b = M.findWithDefault 0 token (corpusHistogram bad)
        ngood = corpusMessages good
        nbad = corpusMessages bad

combine :: [Double] -> Double
combine [] = 0
combine probs = prod / (prod + foldl1' (*) (map (1 -) probs))
  where prod = foldl1' (*) probs

classify :: Corpus -> Corpus -> String -> Double
classify bad good = combine . mapMaybe (probability bad good) . tokenize

spam :: Corpus
spam =
  corpus ["TV Shows Subtitles English TV Subs English Subtitles Subtitles English TV Subs TV Subtitles"
         ,"million tv and movie download links list"
         ,"james may the reassembler s01e04 web dl x264 om"
         ,"james may the reassembler s01e04 web dl x264 om"]

ham :: Corpus
ham =
  corpus ["zmap :: (a->b) -> ([a],[a]) -> ([b],[b])"
         ,"data ListZipper a   = ListZipper {future::[a],past::[a]} "
         ,"instance Show a => Show (ListZipper a) where"]
