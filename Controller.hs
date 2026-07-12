module Controller where

import qualified System.Random.MWC as MWC
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List


import DataVD
import Machine
import Distribution


resampleIndex :: MWC.GenIO -> [Double] -> IO Int
resampleIndex gen probs = do
    u <- MWC.uniform gen
    return (chooseIndex u probs)

runLW :: Expr -> MWC.GenIO -> IO (Value, Double)
runLW program rng = do
    let m = initialMachine program rng
    loop m
    where
    loop m = do
        msg <- resume m
        case msg of
            DoneMsg value machine ->
                return (value, logW machine)

            SampleMsg _ d machine -> do
                value <- sample (Machine.rng machine) d
                loop (send machine value)

            ObserveMsg _ d y machine -> do
                let newMachine = machine { logW = logW machine + logProb d y }
                loop (send newMachine y)

likelihoodWeighting :: Expr -> [MWC.GenIO] -> IO ([Double], [Double])
likelihoodWeighting program rngs = do
    results <- mapM (runLW program) rngs
    let values  = map (valueToDouble . fst) results
        weights = softmax (map snd results)
    return (values, weights)


advance :: Machine -> IO Message
advance m = do
    msg <- resume m
    case msg of
        SampleMsg _ d machine -> do
            value <- sample (rng machine) d
            advance (send machine value)
        _ -> return msg

runSMC :: Expr -> [MWC.GenIO] -> IO [Double]
runSMC program rngs = do
    let particles = zipWith (\i r -> initialMachine program r) [0..] rngs
    loop particles
  where
    loop particles = do
        messages <- mapM advance particles
        case messages of
            [] -> error "No particles"
            _ ->
                if all isDone messages
                then return [valueToDouble v | DoneMsg v _ <- messages]
                else if all isObserve messages
                then smcStep messages
                else error "particles reached different breakpoints"

    smcStep messages = do
        let paused = [m | ObserveMsg _ d y m <- messages]
            increments = [logProb d y | ObserveMsg _ d y _ <- messages]
            probs = softmax increments
        ancestors <- mapM (\_ -> resampleIndex (head rngs) probs) paused
        let newParticles = zipWith (\j i -> fork (Just (rngs !! j)) (paused !! i)) [0..] ancestors
        loop newParticles

mhLogAlpha :: Map.Map [String] Value -> Map.Map [String] Value -> Map.Map [String] Double -> Map.Map [String] Double -> Map.Map [String] Double -> Map.Map [String] Double -> [String] -> Double
mhLogAlpha x x2 s s2 o o2 a0 =
    let fwd = Map.keysSet (Map.singleton a0 ()) `Set.union`(Map.keysSet x2 `Set.difference` Map.keysSet x)
        rev = Map.keysSet (Map.singleton a0 ()) `Set.union` (Map.keysSet x `Set.difference` Map.keysSet x2)
        num = sum [p | (a,p) <- Map.toList s2, not (Set.member a fwd)] + sum (Map.elems o2)
        den = sum [p | (a,p) <- Map.toList s, not (Set.member a rev)] + sum (Map.elems o)
    in log (fromIntegral (Map.size x)) - log (fromIntegral (Map.size x2)) + num - den

runMH :: Expr -> MWC.GenIO -> Maybe [String] -> Map.Map [String] Value -> IO (Value, Map.Map [String] Value, Map.Map [String] Double, Map.Map [String] Double)
runMH program rng selected cache = do
    let m = initialMachine program rng
    loop m Map.empty Map.empty Map.empty
  where
    loop m x s o = do
        msg <- resume m
        case msg of
            SampleMsg addr d machine -> do
                value <- case selected of
                    Just a | a == addr -> sample (Machine.rng machine) d

                    _ -> case Map.lookup addr cache of
                        Just old -> return old
                        Nothing -> sample (Machine.rng machine) d

                let newX = Map.insert addr value x
                    newS = Map.insert addr (logProb d value) s
                loop (send machine value) newX newS o

            ObserveMsg addr d y machine ->
                let newO = Map.insert addr (logProb d y) o
                in loop (send machine y) x s newO

            DoneMsg value machine ->
                return (value, x, s, o)  

singleSiteMH :: Expr -> MWC.GenIO -> Int -> Int -> IO [Double]
singleSiteMH program rng steps warmup = do
    (value, x, s, o) <- runMH program rng Nothing Map.empty
    loop (value, x, s, o) (steps + warmup) []
  where
    loop (value, x, s, o) 0 chain =
        return (reverse chain)

    loop (value, x, s, o) n chain = do
        let addresses = Map.keys x
        index <- MWC.uniformR (0, length addresses - 1) rng
        let a0 = addresses !! index
        (value2, x2, s2, o2) <- runMH program rng (Just a0) x
        let alpha = mhLogAlpha x x2 s s2 o o2 a0
        u <- MWC.uniform rng
        let accepted = log u < alpha
            newState =
                if accepted
                    then (value2, x2, s2, o2)
                    else (value, x, s, o)
            newChain =
                if n <= steps
                    then valueToDouble (fst4 newState) : chain
                    else chain

        loop newState (n - 1) newChain

fst4 (a,_,_,_) = a

isDone :: Message -> Bool
isDone (DoneMsg _ _) = True
isDone _ = False


isObserve :: Message -> Bool
isObserve (ObserveMsg _ _ _ _) = True
isObserve _ = False

finiteSupport :: Distribution -> [(Value, Double)]

finiteSupport (Bernoulli p) =
    [(value, lp) | value <- [Boolean False, Boolean True],let lp = logProb (Bernoulli p) value,lp /= negInf]

finiteSupport _ =
    error "Error in finiteSupport"

enumerateTraces :: Expr -> Int -> IO [(Value, Double)]
enumerateTraces program maxStates = do
    gen <- MWC.createSystemRandom
    let machine = initialMachine program gen
    loop [machine] [] 0
  where
    loop :: [Machine] -> [(Value, Double)] -> Int -> IO [(Value, Double)]
    loop [] finished _ =
        return finished

    loop _ _ visited
        | visited > maxStates =
            error ("state budget exceeded: " ++ show maxStates)

    loop (m : stack) finished visited = do
        msg <- resume m

        case msg of
            DoneMsg value machine ->
                loop stack ((value, logW machine) : finished) (visited + 1)

            ObserveMsg _ d y machine -> do
                let machine' = machine { logW = logW machine + logProb d y }
                loop (send machine' y : stack) finished(visited + 1)

            SampleMsg _ d machine -> do
                let children = [ send(fork Nothing(machine{ logW = logW machine + lp })) value | (value, lp) <- finiteSupport d]
                loop (children ++ stack) finished (visited + 1)

logAddExp :: Double -> Double -> Double
logAddExp a b
    | a == negInf = b
    | b == negInf = a
    | a > b       = a + log (1 + exp (b - a))
    | otherwise   = b + log (1 + exp (a - b))

logSumExp :: [Double] -> Double
logSumExp [] = negInf
logSumExp xs =
    let m = maximum xs
    in m + log (sum (map (exp . subtract m) xs))

posteriorTable :: [(Value, Double)] -> [(String, Double, Double)]
posteriorTable runs =
    let
        logMass = Map.fromListWith logAddExp [(show value, lw) | (value, lw) <- runs]
        entries = Map.toAscList logMass
        z = logSumExp (map snd entries)
    in [(value, lw, exp (lw - z)) | (value, lw) <- entries]



