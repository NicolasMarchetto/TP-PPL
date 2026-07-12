module Distribution where
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist
import qualified Data.Map as Map
import Numeric.SpecFunctions (logGamma)
import DataVD
import System.Random

--Auxiliary functions
poisson :: Double -> MWC.GenIO -> IO Int
poisson lambda gen = do
    let l = exp (-lambda)
    loop l 0 1
  where
    loop l k p
      | p <= l = return (k - 1)
      | otherwise = do
          u <- MWC.uniform gen
          loop l (k + 1) (p * u)

log2pi :: Double
log2pi = log (2 * pi)

sigmoid :: Double -> Double
sigmoid x =
    1 / (1 + exp (-x))

softmax :: [Double] -> [Double]
softmax xs =
    let maxElem = maximum xs
        exps = map (exp . subtract maxElem) xs
        sumExps = sum exps
    in map (/ sumExps) exps

chooseIndex :: Double -> [Double] -> Int
chooseIndex u probs =
    go u 0 0
    where
        go x i acc
            | i >= length probs = i-1
            | x <= acc + probs !! i = i
            | otherwise = go x (i+1) (acc + probs !! i)


valueToDouble :: Value -> Double
valueToDouble (Number x) = x
valueToDouble (Integer x) = fromIntegral x

negInf :: Double
negInf = -1/0



------------------------------------------------------------------
sample :: MWC.GenIO -> Distribution -> IO Value
sample gen distribution =
    case distribution of
        Normal mu sigma ->
            sampleNormal gen mu sigma

        LogNormal mu sigma ->
            sampleLogNormal gen mu sigma

        Uniform a b ->
            sampleUniform gen a b

        Exponential rate ->
            sampleExponential gen rate

        Beta alpha beta ->
            sampleBeta gen alpha beta

        Gamma shape rate ->
            sampleGamma gen shape rate

        Poisson lambda ->
            samplePoisson gen lambda

        Bernoulli p ->
            sampleBernoulli gen p

        Discrete probs ->
            sampleDiscrete gen probs

        UniformDiscrete lo hi ->
            sampleUniformDiscrete gen lo hi

        Dirichlet alphas ->
            sampleDirichlet gen alphas


sampleNormal :: MWC.GenIO -> Double -> Double -> IO Value
sampleNormal gen mu sigma = do
    z <- Dist.normal mu sigma gen
    return $ Number z

sampleLogNormal :: MWC.GenIO -> Double -> Double -> IO Value
sampleLogNormal gen mu sigma = do
    Number x <- sampleNormal gen mu sigma
    return $ Number (exp x)


sampleUniform :: MWC.GenIO -> Double -> Double -> IO Value
sampleUniform gen a b = do
    x <- MWC.uniformR (a,b) gen
    return $ Number x


sampleExponential :: MWC.GenIO -> Double -> IO Value
sampleExponential gen rate = do
    x <- Dist.exponential rate gen
    return $ Number x

sampleBeta :: MWC.GenIO -> Double -> Double -> IO Value
sampleBeta gen alpha beta = do
    x <- Dist.beta alpha beta gen
    return $ Number x


sampleGamma :: MWC.GenIO -> Double -> Double -> IO Value
sampleGamma gen shape rate = do
    x <- Dist.gamma shape (1 / rate) gen
    return $ Number x

samplePoisson :: MWC.GenIO -> Double -> IO Value
samplePoisson gen lambda = do
    x <- poisson lambda gen
    return $ Integer x


sampleBernoulli :: MWC.GenIO -> Double -> IO Value
sampleBernoulli gen p = do
    u <- MWC.uniform gen
    return $ Boolean (u < p)


sampleDiscrete :: MWC.GenIO -> [Double] -> IO Value
sampleDiscrete gen probs = do
    u <- MWC.uniform gen
    return $ Integer (chooseIndex u probs)


sampleUniformDiscrete :: MWC.GenIO -> Int -> Int -> IO Value
sampleUniformDiscrete gen lo hi = do
    x <- MWC.uniformR (lo, hi - 1) gen
    return $ Integer x

sampleDirichlet :: MWC.GenIO -> [Double] -> IO Value
sampleDirichlet gen alphas = do
    values <- mapM (\a -> sampleGamma gen a 1) alphas
    let numbers = [x | Number x <- values]
        total = sum numbers
    return $ Vector (map (Number . (/ total)) numbers)

logProb :: Distribution -> Value -> Double
logProb distribution value =
    case distribution of

        Normal mu sigma ->
            logProbNormal mu sigma value

        LogNormal mu sigma ->
            logProbLogNormal mu sigma value

        Uniform a b ->
            logProbUniform a b value

        Exponential rate ->
            logProbExponential rate value

        Beta alpha beta ->
            logProbBeta alpha beta value

        Gamma shape rate ->
            logProbGamma shape rate value

        Poisson lambda ->
            logProbPoisson lambda value

        Bernoulli p ->
            logProbBernoulli p value

        Discrete probs ->
            logProbDiscrete probs value

        UniformDiscrete lo hi ->
            logProbUniformDiscrete lo hi value

        Dirichlet alphas ->
            logProbDirichlet alphas value

logProbNormal :: Double -> Double -> Value -> Double
logProbNormal mu sigma x =
    let z = (valueToDouble x - mu) / sigma
    in -0.5 * (log2pi + z*z) - log sigma

logProbLogNormal :: Double -> Double -> Value -> Double
logProbLogNormal mu sigma x =
    let v = valueToDouble x
    in if v <= 0
        then negInf
        else
            let z = (log v - mu) / sigma
            in -0.5 * (log2pi + z*z)
               - log sigma
               - log v

logProbUniform :: Double -> Double -> Value -> Double
logProbUniform a b x =
    let v = valueToDouble x
    in if v >= a && v <= b
        then -log (b-a)
        else negInf

logProbExponential :: Double -> Value -> Double
logProbExponential rate x =
    let v = valueToDouble x
    in if v < 0
        then negInf
        else log rate - rate*v

logProbBeta :: Double -> Double -> Value -> Double
logProbBeta alpha beta x =
    let v = valueToDouble x
        logB = logGamma alpha
             + logGamma beta
             - logGamma (alpha + beta)
    in if v <= 0 || v >= 1
        then negInf
        else
            (alpha-1)*log v
            + (beta-1)*log(1-v)
            - logB

logProbGamma :: Double -> Double -> Value -> Double
logProbGamma shape rate x =
    let v = valueToDouble x
    in if v <= 0
        then negInf
        else
            shape * log rate
            - logGamma shape
            + (shape-1)*log v
            - rate*v

logProbPoisson :: Double -> Value -> Double
logProbPoisson lambda x =
    let k = round (valueToDouble x)
    in if k < 0
        then negInf
        else
            fromIntegral k * log lambda
            - lambda
            - logGamma (fromIntegral k + 1)

logProbBernoulli :: Double -> Value -> Double
logProbBernoulli p (Boolean True) =
    if p > 0 then log p else negInf

logProbBernoulli p (Boolean False) =
    if p < 1 then log (1-p) else negInf

logProbBernoulli _ _ =
    negInf

logProbDiscrete :: [Double] -> Value -> Double
logProbDiscrete probs x =
    let k = round (valueToDouble x)
    in if k >= 0
          && k < length probs
          && probs !! k > 0
        then log (probs !! k)
        else negInf

logProbUniformDiscrete :: Int -> Int -> Value -> Double
logProbUniformDiscrete lo hi x =
    let k = round (valueToDouble x)
    in if k >= lo && k < hi
        then -log (fromIntegral (hi-lo))
        else negInf


logProbDirichlet :: [Double] -> Value -> Double
logProbDirichlet alphas (Vector xs)
    | length xs /= length alphas = negInf
    | any (<= 0) numbers         = negInf
    | otherwise                  = sum terms - logB
  where
    numbers = map valueToDouble xs
    terms = zipWith (\a x -> (a - 1) * log x) alphas numbers
    logB = sum (map logGamma alphas)
         - logGamma (sum alphas)

logProbDirichlet _ _ = negInf

params :: Distribution -> [Double]

params (Normal mu sigma) =
    [mu, log sigma]

params (LogNormal mu sigma) =
    [mu, log sigma]

params (Bernoulli p) =
    let p' = min (max p 1e-12) (1 - 1e-12)
    in [log (p' / (1-p'))]

params (Discrete probs) =
    map log probs


params _ =
    error "Distribution is not an optimizable guide"

withParams :: Distribution -> [Double] -> Distribution

withParams (Normal _ _) theta =
    Normal (theta !! 0) (exp (theta !! 1))

withParams (LogNormal _ _) theta =
    LogNormal (theta !! 0) (exp (theta !! 1))

withParams (Bernoulli _) theta =
    Bernoulli (sigmoid (theta !! 0))

withParams (Discrete _) theta =
    Discrete (softmax theta)

withParams _ _ =
    error "Distribution is not an optimizable guide"

gradLogProb :: Distribution -> Value -> [Double]

gradLogProb (Normal mu s) x =
    let z = (valueToDouble x - mu) / s
    in [z / s, z*z - 1]

gradLogProb (LogNormal mu s) x =
    let z = (log $ valueToDouble x - mu) / s
    in [z / s, z*z - 1]

gradLogProb (Bernoulli p) (Boolean x) =
    [fromIntegral (fromEnum x) - p]

gradLogProb (Discrete ps) (Integer k) =
    zipWith (-) [fromIntegral $ fromEnum (i == k) | i <- [0..length ps-1]] ps

gradLogProb _ _ = error "Distribution is not an optimizable guide"


discreteCtor :: [Value] -> Distribution

discreteCtor [Vector xs] =
    Discrete (map valueToDouble xs)

discreteCtor xs =
    Discrete (map valueToDouble xs)


dirichletCtor :: [Value] -> Distribution

dirichletCtor [Vector xs] =
    Dirichlet (map valueToDouble xs)

dirichletCtor xs =
    Dirichlet (map valueToDouble xs)


distributionTable :: Map.Map String ([Value] -> Distribution)

distributionTable =
    Map.fromList
    [
        ("normal", \[Number mu, Number sigma] -> Normal mu sigma),
        ("log-normal", \[Number mu, Number sigma] -> LogNormal mu sigma),
        ("gamma", \[Number shape, Number rate] -> Gamma shape rate),
        ("beta", \[Number a, Number b] -> Beta a b),
        ("exponential", \[Number rate] -> Exponential rate),
        ("uniform", \[Number a, Number b] -> Uniform a b),
        ("poisson", \[Number lambda] -> Poisson lambda),
        ("bernoulli", \[Number p] -> Bernoulli p),
        ("flip", \[Number p] -> Bernoulli p),
        ("discrete", discreteCtor),
        ("categorical", discreteCtor),   
        ("uniform-discrete", \args ->case args of
            [Integer lo, Integer hi] -> UniformDiscrete lo hi
            [Number lo, Number hi]   -> UniformDiscrete (round lo) (round hi)),
        ("dirichlet", dirichletCtor)
    ]

makeGuide :: Distribution -> Distribution

makeGuide (Normal mu sigma) = Normal mu sigma
makeGuide (LogNormal mu sigma) = LogNormal mu sigma
makeGuide (Gamma _ _) = LogNormal 0.0 1.0
makeGuide (Exponential _) = LogNormal 0.0 1.0
makeGuide (Beta _ _) = LogNormal 0.0 1.0
makeGuide (Bernoulli p) = Bernoulli p
makeGuide (Discrete probs) = Discrete probs
makeGuide _ = error "no optimizable guide family for distribution"