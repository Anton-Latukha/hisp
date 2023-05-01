{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Hisp
  ( main
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
 deriving (Eq, Show, Hashable, IsString)

data Expr
  = Var Name
  | App Expr Expr
  | Lambda Name Expr
 deriving Show

data Value
  = Closure (Env Value) Name Expr
 deriving Show

newtype Env a = Env (HashMap Name a)
 deriving (Show, IsList, Semigroup, Monoid, Functor, Foldable, Traversable)

instance One (Env a) where
  type OneItem (Env a) = (Name, a)
  one :: OneItem (Env a) -> Env a
  one = coerce . one @(HashMap Name a)

eval :: MonadFail m => Env Value -> Expr -> m Value
eval e =
  \case
    Var x     ->
      maybe
        (fail "bad")
        pure
        $ M.lookup x $ coerce e
    App f x    ->
      join $ liftA2
        apply
          (eval e f)
          (eval e x)
    Lambda n x ->
      pure $ Closure e n x

data Tar
  = Fresh
  | Wrap Expr

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure e n x) v = eval (one (n, v) <> e) x

program :: MonadFail m => Env Expr -> m (Env Value)
program = traverse (eval mempty)

example :: MonadFail m => m Value
example =
  do
    e <- program $
      fromList [("id", Lambda "x" $ Var "x"), ("const", Lambda "x" $ Lambda "y" $ Var "x")]
    eval e $ App (Var "const") (Var "id")

yCombinator :: MonadFail m => m Value
yCombinator =
  do
   e <- program $ fromList $ one y'
   eval e $ Var "yCombinator"
 where
  x' =
    Lambda "x" $
      App
        (Var "f")
        (App (Var "x") (Var "x"))
  y' =
    ( "yCombinator"
    , Lambda "f" $
        App x' x'
    )

main :: IO ()
main =
  do
    print =<< example
    print =<< yCombinator
