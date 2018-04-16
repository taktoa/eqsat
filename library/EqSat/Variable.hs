--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Variable
  ( Variable
  , fresh
  , freshSimple
  , freshNamed
  , freshLocated
  , freshNamedLocated
  , getID
  , setID
  , getName
  , setName
  , getLoc
  , setLoc
  , structuralEquality
  , structuralOrdering
  , structuralHashWithSalt
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Bool              (Bool, (&&))
import           Data.Eq                (Eq ((==)))
import           Data.Hashable          (Hashable (hashWithSalt))
import           Data.Int               (Int)
import           Data.Loc               (Loc)
import qualified Data.Loc               as Loc
import           Data.Maybe             (Maybe (Just, Nothing))
import           Data.Ord               (Ord (compare), Ordering)
import           Data.Text              (Text)
import           Data.Unique            (Unique, hashUnique, newUnique)

import           GHC.Generics           (Generic)
import           Numeric.Natural        (Natural)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Variable
  = MkVariable
    { variableID   :: !Unique
    , variableName :: !(Maybe Text)
    , variableLoc  :: !(Maybe Loc)
    }
  deriving (Generic)

--------------------------------------------------------------------------------

-- | Note that this only compares the 'Unique' identifier,
--   not the name or location metadata.
instance Eq Variable where
  a == b = (variableID a == variableID b)

-- | Note that this only compares the 'Unique' identifier,
--   not the name or location metadata.
instance Ord Variable where
  compare a b = compare (variableID a) (variableID b)

-- | Note that this only hashes the 'Unique' identifier,
--   not the name or location metadata.
instance Hashable Variable where
  hashWithSalt salt var = salt `hashWithSalt` hashUnique (variableID var)

--------------------------------------------------------------------------------

-- | FIXME: doc
fresh
  :: (MonadIO m)
  => Maybe Text
  -- ^ FIXME: doc
  -> Maybe Loc
  -- ^ FIXME: doc
  -> m Variable
  -- ^ FIXME: doc
fresh name loc = liftIO $ do
  ident <- newUnique
  pure (MkVariable ident name loc)

-- | FIXME: doc
freshSimple
  :: (MonadIO m)
  => m Variable
  -- ^ FIXME: doc
freshSimple = fresh Nothing Nothing

-- | FIXME: doc
freshNamed
  :: (MonadIO m)
  => Text
  -- ^ FIXME: doc
  -> m Variable
  -- ^ FIXME: doc
freshNamed name = fresh (Just name) Nothing

-- | FIXME: doc
freshLocated
  :: (MonadIO m)
  => Loc
  -- ^ FIXME: doc
  -> m Variable
  -- ^ FIXME: doc
freshLocated loc = fresh Nothing (Just loc)

-- | FIXME: doc
freshNamedLocated
  :: (MonadIO m)
  => Text
  -- ^ FIXME: doc
  -> Loc
  -- ^ FIXME: doc
  -> m Variable
  -- ^ FIXME: doc
freshNamedLocated name loc
  = fresh (Just name) (Just loc)

--------------------------------------------------------------------------------

-- | FIXME: doc
getID
  :: Variable
  -- ^ FIXME: doc
  -> Unique
  -- ^ FIXME: doc
getID = variableID

-- | FIXME: doc
setID
  :: Unique
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
setID ident var = var { variableID = ident }

-- | FIXME: doc
getName
  :: Variable
  -- ^ FIXME: doc
  -> Maybe Text
  -- ^ FIXME: doc
getName = variableName

-- | FIXME: doc
setName
  :: Maybe Text
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
setName name var = var { variableName = name }

-- | FIXME: doc
getLoc
  :: Variable
  -- ^ FIXME: doc
  -> Maybe Loc
  -- ^ FIXME: doc
getLoc = variableLoc

-- | FIXME: doc
setLoc
  :: Maybe Loc
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
setLoc loc var = var { variableLoc = loc }

--------------------------------------------------------------------------------

-- | FIXME: doc
structuralEquality
  :: Variable
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Bool
  -- ^ FIXME: doc
structuralEquality a b
  = (    (variableID   a == variableID   b)
      && (variableName a == variableName b)
      && (variableLoc  a == variableLoc  b)
    )

-- | FIXME: doc
structuralOrdering
  :: Variable
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Ordering
  -- ^ FIXME: doc
structuralOrdering a b
  = let fromLoc :: Loc -> (Loc.Line, Loc.Column)
        fromLoc loc = (Loc.locLine loc, Loc.locColumn loc)
    in compare
       (variableID a, variableName a, fromLoc <$> variableLoc a)
       (variableID b, variableName b, fromLoc <$> variableLoc b)

-- | FIXME: doc
structuralHashWithSalt
  :: Int
  -- ^ FIXME: doc
  -> Variable
  -- ^ FIXME: doc
  -> Int
  -- ^ FIXME: doc
structuralHashWithSalt salt var
  = let fromLoc :: Loc -> (Natural, Natural)
        fromLoc loc = ( Loc.toNat (Loc.locLine loc)
                      , Loc.toNat (Loc.locColumn loc) )
    in salt
       `hashWithSalt` hashUnique (variableID var)
       `hashWithSalt` variableName var
       `hashWithSalt` (fromLoc <$> variableLoc var)

--------------------------------------------------------------------------------
