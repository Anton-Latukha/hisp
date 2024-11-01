{-# language PatternSynonyms #-}
{-# language DeriveAnyClass #-}

module Hisp where

import           Hisp.Prelude hiding (LT)
import Relude.Extra.Enum (next)
import qualified Text.Show
import Data.Attoparsec.Text
    ( decimal, parseTest, char, parseOnly, string, Parser )
import           Test.QuickCheck.Gen
import           Data.GenValidity
import Yaya.Fold ( Steppable(..), Projectable(..), Mu(..) )
import           Data.Functor.Classes

-- ** Lambda calculi

-- *** Initial type primitive boundaries

-- | Bruijn index in lambda term.
-- Index < number of external lambda binds => index == binded lambda value
-- Index >= number of external lambda binds => index == free variable
newtype BJIndx = BJIndx Int
 deriving (Eq, Enum, Num, Bounded, Show, Generic, Validity, GenValid)

newtype LTFAppFunc a = LTFAppFunc (LTF a)
 deriving (Eq, Eq1, Show, Generic, Functor, Traversable, Foldable, Validity, GenValid)

newtype LTFAppParam a = LTFAppParam (LTF a)
 deriving (Eq, Eq1, Show, Generic, Functor, Traversable, Foldable, Validity, GenValid)

newtype LTFLamBody a = LTFLamBody (LTF a)
 deriving (Eq, Eq1, Show, Generic, Functor, Traversable, Foldable, Validity, GenValid)

data LTF a
  = LTFBJIndx !BJIndx
  | LTFApp    !(LTFAppFunc a) !(LTFAppParam a)
  | LTFLam    !(LTFLamBody a)
 deriving (Eq, Show, Generic, Functor, Traversable, Foldable, Validity, GenValid, Projectable (->) LT, Steppable (->) LT)

instance Eq1 LTF where
  liftEq :: (a -> b -> Bool) -> LTF a -> LTF b -> Bool
  liftEq = go  -- Making shure GHC detects that there is no point to go through typeclass dictionary searches, all other instances derive from here.
   where
    go eq (LTFLam    b1   ) (LTFLam    b2   ) = crc go eq b1 b2
    go eq (LTFApp    f1 p1) (LTFApp    f2 p2) = crc go eq f1 f2 && crc go eq p1 p2
    go _  (LTFBJIndx idx1 ) (LTFBJIndx idx2 ) = idx1 == idx2
    go _ _ _ = False

newtype LT = LT (Mu LTF)
 deriving (Eq, Generic)

-- *** Isomorphism of lambda term to human readable representation

-- | Abstraction for representation of human readable view of the main lambda term datatype
newtype LTBJHumanReadable = LTBJHumanReadable LT
instance Show LTBJHumanReadable where
  show :: LTBJHumanReadable -> String
  show = l_showHR . crc
   where
    -- | There is a newtype boundary between main lambda term data type and human readable, code prefers to preserve the general GHC derived @Show@ instances for the general case (showing term/expression internals) for the lambda term and its components, which is why this coersion enforsment is needed.
    l_showHR :: LT -> String
    l_showHR =
      caseLT
        show
        showApp
        showLam
     where
      showApp :: LT -> LT -> String
      showApp f a = "(" <> l_showHR f <> ") " <> l_showHR a
      showLam :: LT -> String
      showLam b = "\\ " <> l_showHR b

turnReadable :: LT -> Text
turnReadable = show . LTBJHumanReadable

instance Show LT where
  show (crc @LTBJHumanReadable -> a) = show a

-- *** Main data type

instance Validity LT where
  validate :: LT -> Validation
  validate lt =
    check
      (isRight $ turnReadableThenParseBack lt)
      ("Noop" <> show lt <> ".")
    <>
    check
      ((==) (fromRight mk0 $ turnReadableThenParseBack lt) lt)
      ("Noop" <> show lt <> ".")
  
-- genLTValidity :: Gen Validation
-- genLTValidity = fmap validate (genValid @LT)

-- *** Patterns

pattern PatLTBJIndx :: Int -> LT
pattern PatLTBJIndx n <- (project -> LTFBJIndx (BJIndx n)) where
        PatLTBJIndx n =     embed (  LTFBJIndx (BJIndx n))

pattern PatLTApp :: LT -> LT -> LT
pattern PatLTApp f a <- (project -> LTFApp (LTFAppFunc (embed -> f)) (LTFAppParam (embed -> a))) where
        PatLTApp f a =     embed (  LTFApp (LTFAppFunc (project  f)) (LTFAppParam (project  a)))

pattern PatLTLam :: LT -> LT
pattern PatLTLam b <- (project -> LTFLam (LTFLamBody (embed -> b))) where
        PatLTLam b =     embed (  LTFLam (LTFLamBody (project  b)))

{-# complete PatLTBJIndx, PatLTApp, PatLTLam #-}

-- *** Builders

mkLTBJIndx :: Int -> LT
mkLTBJIndx = PatLTBJIndx

mkLTApp :: LT -> LT -> LT
mkLTApp = PatLTApp

mkLTLam :: LT -> LT
mkLTLam = PatLTLam

-- *** Helpers

-- | Takes a set of for lambda term cases and applies according function:
caseLT
  :: (Int -> a)     -- ^ For index
  -> (LT -> LT -> a) -- ^ For application
  -> (LT -> a)      -- ^ For function
  -> LT            -- ^ Term
  -> a             -- ^ Result of the accordingly applied function
caseLT cf ca cl =
 \case
  PatLTBJIndx i -> cf   i
  PatLTApp  f a -> ca f a
  PatLTLam    b -> cl   b

-- *** Parser

parserLT :: Parser LT
parserLT =
  bruijnIndexParser <|>
  lambdaParser <|>
  appParser
 where
  bruijnIndexParser :: Parser LT
   = mkLTBJIndx <$> decimal
  lambdaParser :: Parser LT
   = mkLTLam <$> (string "\\ " *> bruijnIndexParser)
  appParser :: Parser LT
   = mkLTApp <$> appFuncParser <*> appParamParser
   where
    appFuncParser :: Parser LT
     = char '(' *> parserLT <* char ')'
    appParamParser :: Parser LT
     = char ' ' *> parserLT

-- *** Testing

runOutputUnitTests :: IO ()
runOutputUnitTests =
  traverse_
    (putTextLn . turnReadable)
    lambdaTermUnitTests

-- | Parses only lawful Bruijin lambda terms.
runParserUnitTests :: IO ()
runParserUnitTests =
  traverse_ (parseTest parserLT . (<> "\\n") . turnReadable) lambdaTermUnitTests

lambdaTermUnitTests :: Seq LT
lambdaTermUnitTests =
  (<>)
    (one mk0)
    ((`mkLTApp` mk0) <$>
      [ mk0
      , PatLTLam mk0
      , PatLTLam mk0
      ]
    )

mk0 :: LT
mk0 = mkLTBJIndx 0

turnReadableThenParseBack :: LT -> Either String LT
turnReadableThenParseBack = parseOnly parserLT . (<> "\\n") . turnReadable

-- testRoundTripLTShowAndBack :: Gen (Either String LT)
-- testRoundTripLTShowAndBack = fmap turnReadableThenParseBack genValid

-- fun1 :: IO ()
-- fun1 = print =<< generate genLTValidity

-- | Normal form lambda term.
newtype NLT = NLT LT

normalize :: LT -> NLT
normalize = crc .
  caseLT
    PatLTBJIndx
    (flip betaReduce)
    (PatLTLam . crc . normalize)
 where
  -- | Lambda function application.
  -- Does beta-reduce when lambda term matches definition, otherwise does id.
  -- TODO: Try for this function to return Maybe.
  betaReduce
    :: LT -- ^ Argument to bind
    -> LT -- ^ Expression to find bind targets
    -> LT -- ^ Expression with the bind applied
  betaReduce a =
    \case
      (PatLTLam lb) -> substitute a 0 lb -- run beta reduction operation only when it matches definition.
      other -> PatLTApp other a
   where
    substitute :: LT -> BJIndx -> LT -> LT
    substitute v bji =
      caseLT
        (bool v . PatLTBJIndx <*> (crc bji /=))
        (on PatLTApp (substitute v bji))
        (substitute v $! next bji) -- Going inside internal lambda term - increase Bruijn Index code searches for.
