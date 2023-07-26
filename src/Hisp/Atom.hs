{-# language CPP               #-}
{-# language DeriveAnyClass    #-}

module Hisp.Atom where

import           Hisp.Prelude
import           Codec.Serialise                ( Serialise )

import           Data.Data                      ( Data)
import           Data.Fixed                     ( mod' )
import           Data.Binary                    ( Binary )
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON
                                                )
--  2021-08-01: NOTE: Check the order effectiveness of HispAtom constructors.

-- | Atoms are values that evaluate to themselves.
-- In other words - constructors that are literals in the language.
-- Literals appear in both the parsed AST (in the form of literals) and
-- the evaluated form as themselves.
-- Once Hisp parsed or evaluated into atom - that is a literal
-- further after, for any further evaluation it is in all cases stays
-- constantly itself.
-- "atom", Ancient Greek \( atomos \) - "indivisible" particle,
-- indivisible expression.
data HispAtom
  -- | An URI like @https://example.com@.
  = HispURI Text
  -- | An integer (@Int64@).
  | HispInt Integer
  -- | A floating point number.
  | HispFloat Float
  -- | Booleans. @False@ or @True@.
  | HispBool Bool
  -- | Null value. For development purpose, should be radically removed in the stable versions of language.
  | HispNull
  deriving
    ( Eq
    , Ord
    , Generic
    , Typeable
    , Data
    , Show
    , Read
    , NFData
    , Hashable
    )

instance Serialise HispAtom

instance Binary HispAtom
instance ToJSON HispAtom
instance FromJSON HispAtom

-- | Translate an atom into its Nix representation.
atomText :: HispAtom -> Text
atomText (HispURI   t) = t
atomText (HispInt   i) = show i
atomText (HispFloat f) = showFloat f
 where
  showFloat :: Float -> Text
  showFloat x =
    bool
      (show x)
      (show (truncate x :: Int))
      (x `mod'` 1 == 0)
atomText (HispBool  b) = bool "False" "True" b
atomText HispNull      = "Null"
