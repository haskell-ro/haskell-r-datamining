import Data.List
import System.Random

data Nucleotide = A | C | G | T deriving (Eq, Show, Enum, Bounded)
type Sequence = [Nucleotide]
type DataPoints = [Sequence]
type Cluster = [Sequence]
type Clustering = [Cluster]

instance Random Nucleotide where
  random = randomR (minBound :: Nucleotide, maxBound)
  randomR (min, max) g = (toEnum v, g')
    where (v, g') = randomR (fromEnum min, fromEnum max) g

generateSequence :: RandomGen g => g -> Int -> Sequence
generateSequence g sz = take sz $ randoms g

generateDataPoints :: RandomGen g => g -> Int -> Int -> DataPoints
generateDataPoints g n sz = take n $ map (flip generateSequence sz) generators
  where
    generators = map mkStdGen $ randoms g

distance :: Sequence -> Sequence -> Double
distance [] [] = 0
distance [] _ = 1
distance _ [] = 1
distance (x:xs) (y:ys)
  | x == y = distance xs ys / 2
  | otherwise = 1

cluster :: Double -> DataPoints -> Clustering
cluster _ [] = []
cluster a (p:pts) = (p:cls) : cluster a other
  where
    (cls, other) = partition (\x -> distance p x <= a) pts

-- From ghci, call with
-- > cluster 0.5 (generateDataPoints (mkStdGen 42) 100 10)
