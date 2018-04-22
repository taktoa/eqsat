--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Errors.CheckEquationError
  ( CheckEquationError ( InferenceFailure
                       , _InferenceFailure_lhs
                       , _InferenceFailure_rhs
                       , _InferenceFailure_side
                       , _InferenceFailure_error
                       , OutOfScope
                       , _OutOfScope_var
                       , _OutOfScope_lhs
                       , _OutOfScope_rhs
                       , MetaVarNotSubtype
                       , _MetaVarNotSubtype_var
                       , _MetaVarNotSubtype_lhs
                       , _MetaVarNotSubtype_rhs
                       , _MetaVarNotSubtype_lhsVarType
                       , _MetaVarNotSubtype_rhsVarType
                       , OverallNotEqual
                       , _OverallNotEqual_lhs
                       , _OverallNotEqual_rhs
                       , _OverallNotEqual_lhsType
                       , _OverallNotEqual_rhsType
                       , Impossible
                       , _Impossible_lhs
                       , _Impossible_rhs
                       , _Impossible_message
                       )
    -- FIXME: don't export these constructors, instead export throw* functions,
    --        lenses, and prisms.
  , EquationSide (EquationSideLHS, EquationSideRHS)
  ) where

--------------------------------------------------------------------------------

import           Data.Hashable                (Hashable)
import           Data.Void                    (Void)
import           GHC.Generics                 (Generic)

import qualified EqSat.Internal.PrettyPrinter as PP
import           EqSat.Term                   (GTerm, TTerm)
import           EqSat.TypedTerm              (TypedGTerm, TypedTTerm)
import           EqSat.TypeSystem             (Type, TypeError)

--------------------------------------------------------------------------------

-- | FIXME: doc
data CheckEquationError node var expr
  = -- | FIXME: doc
    InferenceFailure
    { _InferenceFailure_lhs   :: !(TTerm node var)
    , _InferenceFailure_rhs   :: !(GTerm node var)
    , _InferenceFailure_side  :: !EquationSide
    , _InferenceFailure_error :: !(TypeError expr)
    }
  | -- | FIXME: doc
    OutOfScope
    { _OutOfScope_var :: !var
    , _OutOfScope_lhs :: !(TypedTTerm node var (Type expr))
    , _OutOfScope_rhs :: !(TypedGTerm node var (Type expr))
    }
  | -- | FIXME: doc
    MetaVarNotSubtype
    { _MetaVarNotSubtype_var        :: !var
    , _MetaVarNotSubtype_lhs        :: !(TypedTTerm node var (Type expr))
    , _MetaVarNotSubtype_rhs        :: !(TypedGTerm node var (Type expr))
    , _MetaVarNotSubtype_lhsVarType :: !(Type expr)
    , _MetaVarNotSubtype_rhsVarType :: !(Type expr)
    }
  | -- | FIXME: doc
    OverallNotEqual
    { _OverallNotEqual_lhs     :: !(TypedTTerm node var (Type expr))
    , _OverallNotEqual_rhs     :: !(TypedGTerm node var (Type expr))
    , _OverallNotEqual_lhsType :: !(Type expr)
    , _OverallNotEqual_rhsType :: !(Type expr)
    }
  | -- | FIXME: doc
    Impossible
    { _Impossible_lhs     :: !(TTerm node var)
    , _Impossible_rhs     :: !(GTerm node var)
    , _Impossible_message :: !(PP.Doc Void)
    }
  deriving (Generic)

-- FIXME: write lenses and prisms for CheckEquationError

--------------------------------------------------------------------------------

-- FIXME: maybe move into an `EqSat.Utility` module?

-- | A datatype representing one side of an equation.
data EquationSide
  = -- | The left-hand side of an equation.
    EquationSideLHS
  | -- | The right-hand side of an equation.
    EquationSideRHS
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME: doc
instance Hashable EquationSide

--------------------------------------------------------------------------------
