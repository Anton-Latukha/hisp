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
  | Neutral Neutral
 deriving Show

newtype Neutral
  = NVar Name
 deriving Show

newtype Env a = Env (HashMap Name a)
 deriving (Show, IsList, Semigroup, Monoid, Functor, Foldable, Traversable)

instance One (Env a) where
  type OneItem (Env a) = (Name, a)
  one :: OneItem (Env a) -> Env a
  one = coerce . one @(HashMap Name a)

eval :: MonadFail m => Env Value -> Expr -> m Value
eval env =
  \case
    Var x     ->
      maybe
        (fail "bad")
        pure
        $ M.lookup x $ coerce env
    App f x    ->
      join $ liftA2
        apply
          (eval env f)
          (eval env x)
    Lambda n x ->
      pure $ Closure env n x

type Namespace = [Name]

--  2023-05-04: FIXME: Find a way to remove this aglines, maybe with type abstraction.
fresh :: Namespace -> Name -> Name
fresh xs x
  bool
    x
    (fresh xs (x <> "'"))
    (x `elem` xs)

-- | A funny name for a function that turns value back into readable expression.
valueExpress :: MonadFail m => Namespace -> Value -> m Expr
valueExpress xs (Closure env x b) =
  do
    let
      x' = fresh xs x
    bv <- eval (one (x, Neutral $ NVar x') <> env) b
    Lambda x' <$> valueExpress (one x' <> xs) bv
valueExpress _ (Neutral n) = valueExpressNeutral n

valueExpressNeutral :: MonadFail m => Neutral -> m Expr
valueExpressNeutral (NVar x) = pure $ Var x

nf :: MonadFail m => Env Value -> Expr -> m Expr
nf env t =
  do
    v <- eval env t
    valueExpress v

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure env n x) v = eval (one (n, v) <> env) x

-- | Builds the environment,
-- embodies which variables and functions are in the scope of something.
scope :: MonadFail m => Env Expr -> m (Env Value)
scope = traverse (eval mempty)

example :: MonadFail m => m Value
example =
  do
    env <- scope $
      fromList [("id", Lambda "x" $ Var "x"), ("const", Lambda "x" $ Lambda "y" $ Var "x")]
    eval env $ App (Var "const") (Var "id")

yCombinator :: MonadFail m => m Value
yCombinator =
  do
   env <- scope $ fromList $ one y'
   eval env $ Var "yCombinator"
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
