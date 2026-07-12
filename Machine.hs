module Machine where

import qualified Data.Map as Map
import qualified System.Random.MWC as MWC

import DataVD
import qualified Primitive
import qualified Distribution

data Frame
    = Ev Expr Env [String]
    | LetK [Expr] Int [Expr] Env [String]
    | IfK Expr Expr Env [String]
    | Discard
    | CallK Int [String]
    | SampleK [String]
    | ObserveK [String]
    deriving (Show)


data Machine = Machine
    { control :: [Frame]
    , values  :: [Value]
    , env     :: Env
    , rng     :: MWC.GenIO
    , logW    :: Double
    }


data Message
    = SampleMsg [String] Distribution Machine
    | ObserveMsg [String] Distribution Value Machine
    | DoneMsg Value Machine



initialMachine :: Expr -> MWC.GenIO -> Machine
initialMachine program gen =
    Machine
        { control = [Ev program Map.empty []]
        , values = []
        , env = Map.empty
        , rng = gen
        , logW = 0.0
        }



fork :: Maybe MWC.GenIO -> Machine -> Machine
fork newGen m =
    m { rng = maybe (rng m) id newGen }



send :: Machine -> Value -> Machine
send m v =
    m { values = v : values m }


resume :: Machine -> IO Message

resume m@Machine{control=[], values=(v:_)} =
    return (DoneMsg v m)

resume m@Machine{control=(instr:rest)} = do
    putStrLn ("FRAME: " ++ show instr)
    putStrLn ("VALUES: " ++ show (values m))
    case instr of

        Ev expr env addr ->
            evalExpr expr env addr m { control = rest }

        LetK link i body env addr ->
            let value = head (values m)
                remaining = tail (values m)
                name =
                    case link !! (2*i) of
                        Symbol s -> s
                        _ -> error "let binding is not symbol"
                newEnv = Map.insert name value env
            in
                if 2*(i+1) < length link
                then
                    resume m
                        { control =
                            Ev (link !! (2*(i+1)+1)) newEnv addr :
                            LetK link (i+1) body newEnv addr :
                            rest
                        , values = remaining
                        }
                else
                    resume m
                        { control = map (\e -> Ev e newEnv addr) body ++ rest
                        , env = newEnv
                        , values = remaining
                        }

        IfK thenExpr elseExpr env addr -> do
            putStrLn "------IFK ---"
            putStrLn ("Values: " ++ show (values m))
            case values m of
                (condition:remaining) -> do
                    let branch =
                            case condition of
                                Boolean True -> thenExpr
                                _ -> elseExpr
                    resume m
                        { control = Ev branch env addr : rest
                        , values = remaining
                        }
                [] ->
                    error "IFK receive empty values stack"

        Discard ->
            resume m
                { control = rest
                , values = tail (values m)
                }

        CallK n addr ->
            let args = reverse (take n (values m))
                remaining = drop n (values m)
                f = head remaining
                stack = tail remaining
            in
                applyFunction f args m
                    { control = rest
                    , values = stack
                    }

        SampleK addr ->
            case values m of
                (DistValue d : restValues) ->
                    return
                        (SampleMsg addr d m
                            { values = restValues
                            , control = tail (control m)
                            })
                _ ->
                    error "Sample expected distribution"

        ObserveK addr ->
            case values m of
                (value : DistValue d : restValues) ->
                    return
                        (ObserveMsg addr d value m
                            { values = restValues
                            , control = tail (control m)
                            })
                _ ->
                    error "Observe expected distribution and value"

evalExpr :: Expr -> Env -> [String] -> Machine -> IO Message

evalExpr expr env addr m =
    case expr of

        Symbol name ->
            case Map.lookup name env of
                Just value ->
                    resume m { values = value : values m }
                Nothing ->
                    case Map.lookup name Distribution.distributionTable of
                        Just _ ->
                            resume m
                                { values = DistConstructor name : values m
                                }
                        Nothing ->
                            case Map.lookup name Primitive.primitives of
                                Just prim ->
                                    resume m
                                        { values = Primitive prim : values m
                                        }
                                Nothing ->
                                    error ("Unknown symbol: " ++ name)

        NumberLit x ->
            resume m { values = Number x : values m }

        IntegerLit x ->
            resume m { values = Integer x : values m }

        BooleanLit x ->
            resume m { values = Boolean x : values m }

        List xs ->
            evalList xs env addr m

        _ ->
            error "unsupported expression"

evalList :: [Expr] -> Env -> [String] -> Machine -> IO Message

evalList [] _ _ _ =
    error "empty expression"

evalList (Symbol "let":binds:body) env addr m =
    case binds of
        List xs ->
            resume m
                { control =
                    Ev (xs !! 1) env addr :
                    LetK xs 0 body env addr :
                    control m
                }

        _ ->
            error "invalid let"

evalList (Symbol "if":test:thenE:elseE:_) env addr m =
    resume m
        { control =
            Ev test env addr :
            IfK thenE elseE env addr :
            control m
        }

evalList (Symbol "sample":dist:_) env addr m =
    resume m
        { control =
            Ev dist env addr :
            SampleK addr :
            control m
        }

evalList (Symbol "observe":dist:value:_) env addr m =
    resume m
        { control =
            Ev dist env addr :
            Ev value env addr :
            ObserveK addr :
            control m
        }

evalList exprs env addr m =
    let
        fn = head exprs
        args = tail exprs
        n = length args
        instructions =
            Ev fn env addr :
            map (\e -> Ev e env addr) args

    in

    resume m
        { control =
            instructions ++
            [CallK n addr] ++
            control m
        }

applyFunction :: Value -> [Value] -> Machine -> IO Message


applyFunction (Primitive f) args m =
    resume m
        { values = f args : values m
        }


applyFunction (DistConstructor name) args m =
    case Map.lookup name Distribution.distributionTable of
        Just constructor ->
            resume m
                { values =
                    DistValue (constructor args)
                    : values m
                }
        Nothing ->
            error "unknown distribution"


applyFunction (ClosureV closure) args m =
    let newEnv =
            Map.union
                (Map.fromList (zip (params closure) args))
                (cEnv closure)
    in resume m
        { control =
            map (\e -> Ev e newEnv []) (body closure)
            ++ control m
        }


applyFunction _ _ _ =
    error "not callable"

distributionFromValue :: Value -> Distribution

distributionFromValue (DistValue d) = d

distributionFromValue _ =
    error "Value is not a distribution"
