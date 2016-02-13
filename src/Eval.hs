module Eval where

import Core

eval :: Env -> Form -> (Env, Value)
eval env (Atom (Symbol sym)) = (env, lookupEnv env sym)
eval env (Atom val) = (env, AtomValue val)
