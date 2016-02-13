module Core where

import qualified Data.Map as M

data Sym =
    Sym String
    deriving (Show,Eq,Ord)

data Atom
    = Nil
    | Bool Bool
    | String String
    | Int Int
    | Symbol Sym
    deriving (Show,Eq,Ord)

data Form
    = SExp [Form]
    | Atom Atom
    deriving (Show,Eq,Ord)

data Lambda = Lambda
    { args :: [Sym]
    , body :: Form
    , env :: Env
    } deriving (Show,Eq,Ord)

data Value
    = AtomValue Atom
    | ListValue [Value]
    | LambdaValue Lambda
    deriving (Show,Eq,Ord)

data Env = Env
    { bindings :: M.Map Sym Value
    , parent :: Maybe Env
    } deriving (Show,Eq,Ord)


emptyEnv :: Env
emptyEnv =
    Env
    { bindings = M.empty
    , parent = Nothing
    }

pushEnv :: Env -> [(Sym, Value)] -> Env
pushEnv env bindings =
    Env
    { bindings = M.fromList bindings
    , parent = Just env
    }

popEnv :: Env -> Env
popEnv env = case parent env of
  Just env -> env
  Nothing  -> error "Trying to pop the root env" -- TODO A better way to handle this case ?

-- Lookup a binding in a Env
lookupEnv :: Env -> Sym -> Value
lookupEnv env sym =
    case M.lookup sym (bindings env) of
        Just val -> val
        Nothing ->
            case (parent env) of
                Just p -> lookupEnv p sym
                Nothing -> AtomValue Nil
