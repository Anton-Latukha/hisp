{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Hisp
  ( main
  )
where

import           Hisp.Prelude

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
  = NeutralVar Name
  | NeutralApp Neutral Value
 deriving Show

newtype Env a = Env (HashMap Name a)
 deriving
   ( Show
   , IsList
   , Semigroup
   , Monoid
   , Functor
   , Foldable
   , Traversable
   )

instance One (Env a) where
  type OneItem (Env a) = (Name, a)
  one :: OneItem (Env a) -> Env a
  one = coerce . one @(HashMap Name a)

eval :: MonadFail m => Env Value -> Expr -> m Value
eval env =
  \case
    Var name         ->
      maybe
        (fail "bad")
        pure
        (lookupHM (coerce env) name)
    App binds expr   ->
      join $
        liftA2
          apply
          (eval env binds)
          (eval env expr)
    Lambda name expr ->
      pure $ Closure env name expr

evalM :: MonadFail m => m (Env Value) -> Expr -> m Value
evalM b e =
  (`eval` e) =<< b

-- | 2023-07-27: FIXME: Migrate from HashMap to HashSet
type Namespace = HashMap Name ()

--  2023-05-04: FIXME: Find a way to remove this uglines, maybe with type abstraction.
createUnique :: Namespace -> Name -> Name
createUnique namespace name =
  maybe
    name
    (const $ createUnique namespace (name <> "'"))
    (lookupHM namespace name)

-- | A funny name for a function that turns value back into readable expression.
expressValue :: MonadFail m => Namespace -> Value -> m Expr
expressValue namespace (Closure env name expr) =
  (<$>)
    (Lambda uniqueName)
    (expressValue
      (cons
        (uniqueName, mempty)
        namespace
      )
      =<<
        eval
          (cons (name, Neutral $ NeutralVar uniqueName) env)
          expr
    )
 where
  uniqueName = createUnique namespace name
expressValue namespace (Neutral n) = expressValueNeutral namespace n

expressValueNeutral :: MonadFail m => Namespace -> Neutral -> m Expr
expressValueNeutral namespace =
  \case
    NeutralVar x   ->
      pure $ Var x
    NeutralApp f x ->
      liftA2
        App
        (expressValueNeutral namespace f)
        (expressValue        namespace x)

normalForm :: MonadFail m => Env Value -> Expr -> m Expr
normalForm env t =
  expressValue mempty =<< eval env t

apply :: MonadFail m => Value -> Value -> m Value
apply (Closure env name expr) val = eval (cons (name, val) env) expr
apply (Neutral neutral      ) val = pure $ Neutral $ NeutralApp neutral val

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
  evalM (scope $ fromList (one yCombinator) <> basisExpr) $ Var "yCombinator"
 where
  x' =
    Lambda "x" $
      App
        (Var "f")
        (App (Var "x") (Var "x"))
  yCombinator =
    ( "yCombinator"
    , Lambda "f" $
        App x' x'
    )

printValue :: (MonadFail m, MonadIO m) => m Value -> m ()
printValue e = print =<< expressValue mempty =<< e

main :: IO ()
main =
  do
    printValue example
    printValue yCombinator
