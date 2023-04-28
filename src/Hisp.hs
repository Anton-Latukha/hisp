{-# language TypeFamilies #-}

module Hisp
  (
  )
where

import           Hisp.Prelude
import           Relude.Unsafe                  ( (!!) )
import           GHC.Err                        ( errorWithoutStackTrace )
import           Data.Fix                       ( Fix )
import qualified Data.HashMap.Lazy             as M
import qualified Data.Text                     as Text
import qualified Data.Text.Read                as Text

newtype Name = Name Text
 deriving (Eq, Show, Hashable)

data Expr
  = Var Name
  | App Expr Expr
  | Lambda Name Expr
 deriving Show

data Value
  = Closure Env Name Expr
 deriving Show

newtype Env = Env (HashMap Name Value)
 deriving (Show, Semigroup, Monoid)

instance One Env where
  type OneItem Env = (Name, Value)
  one (x, y) = Env $ one (x, y)

eval :: MonadFail m => Env -> Expr -> m Value
eval e (Var x) =
  maybe
    (fail "bad")
    pure
    $ M.lookup x $ coerce e
eval e (App f x) =
  do
    fv <- eval e f
    xv <- eval e x
    apply fv xv
eval e (Lambda n x) = pure $ Closure e n x

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure e n x) v = eval (one (n, v) <> e) x
