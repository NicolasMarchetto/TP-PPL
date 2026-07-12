module DataVD where
import qualified Data.Map as Map

-- Expr represents the program syntax before evaluation while Value represents the result after evaluation.You can think of Expr as the "code" and Value as the "data" that the code produces


type Env = Map.Map String Value

data Expr
    = Symbol String
    | NumberLit Double
    | IntegerLit Int
    | BooleanLit Bool
    | StringLit String
    | Nil
    | List [Expr]
    deriving (Show, Eq)


data Closure = Closure
    { params :: [String]
    , body   :: [Expr]
    , cEnv   :: Env
    }

data Distribution
    = Normal Double Double
    | LogNormal Double Double
    | Uniform Double Double
    | Exponential Double
    | Beta Double Double
    | Gamma Double Double
    | Poisson Double
    | Bernoulli Double
    | Discrete [Double]
    | UniformDiscrete Int Int
    | Dirichlet [Double]
    deriving (Show, Eq)

data Value
    = Number Double
    | Integer Int
    | Boolean Bool
    | Vector [Value]
    | Matrix [[Double]]
    | HashMap (Map.Map String Value)
    | Primitive ([Value] -> Value)
    | ClosureV Closure
    | DistValue Distribution
    | DistConstructor String


instance Show Value where
    show (Number x) = show x
    show (Integer x) = show x
    show (Boolean b) = show b
    show (Vector xs) = show xs
    show (Matrix m) = show m
    show (HashMap h) = show h
    show (Primitive _) = "<primitive>"
    show (ClosureV _) = "<closure>"
    show (DistValue _) = "<distribution>"
    show (DistConstructor s) = "<distribution constructor " ++ s ++ ">"

instance Eq Value where
    Number a == Number b = a == b
    Integer a == Integer b = a == b
    Boolean a == Boolean b = a == b
    Vector a == Vector b = a == b
    Matrix a == Matrix b = a == b
    HashMap a == HashMap b = a == b
    _ == _ = False
