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
 deriving (Eq, Show, Hashable, IsString, Semigroup)

data Expr
  = Var Name
  | App Expr Expr
  | Lambda Name Expr
 deriving Show

data Value
  = Closure (Env Value) Name Expr
  | Neutral Neutral
 deriving Show

data Neutral
  = NVar Name
  | NApp Neutral Value
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
      join $
        liftA2
          apply
            (eval env f)
            (eval env x)
    Lambda n x ->
      pure $ Closure env n x

evalM :: MonadFail m => m (Env Value) -> Expr -> m Value
evalM b e =
  b >>= (`eval` e)

type Namespace = [Name]

--  2023-05-04: FIXME: Find a way to remove this aglines, maybe with type abstraction.
fresh :: Namespace -> Name -> Name
fresh xs x =
  bool
    x
    (fresh xs (x <> "'"))
    (x `elem` xs)

-- | A funny name for a function that turns value back into readable expression.
expressValue :: MonadFail m => Namespace -> Value -> m Expr
expressValue xs (Closure env x b) =
  do
    let
      x' = fresh xs x
    bv <- eval (one (x, Neutral $ NVar x') <> env) b
    Lambda x' <$> expressValue (one x' <> xs) bv
expressValue xs (Neutral n) = expressValueNeutral xs n

expressValueNeutral :: MonadFail m => Namespace -> Neutral -> m Expr
expressValueNeutral xs =
  \case
    NVar x   ->
      pure $ Var x
    NApp f x ->
      App
        <$> expressValueNeutral xs f
        <*> expressValue xs x

nf :: MonadFail m => Env Value -> Expr -> m Expr
nf env t =
  expressValue mempty =<< eval env t

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure env n x) v = eval (one (n, v) <> env) x
apply (Neutral n) v = pure $ Neutral $ NApp n v

-- | Builds the environment,
-- embodies which variables and functions are in the scope of something.
scope :: MonadFail m => Env Expr -> m (Env Value)
scope = traverse (eval mempty)

basisExpr :: Env Expr
basisExpr =
  fromList [("id", Lambda "x" $ Var "x"), ("const", Lambda "x" $ Lambda "y" $ Var "x")]

basisScope :: MonadFail m => m (Env Value)
basisScope =
  scope basisExpr

example :: MonadFail m => m Value
example =
  evalM basisScope $ App (Var "const") (Var "id")

yCombinator :: MonadFail m => m Value
yCombinator =
  evalM (scope $ fromList (one y') <> basisExpr) $ Var "yCombinator"
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
    print =<< expressValue mempty =<< example
    print =<< expressValue mempty =<< yCombinator
