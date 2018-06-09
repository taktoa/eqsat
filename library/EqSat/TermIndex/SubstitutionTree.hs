--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.TermIndex.SubstitutionTree
  ( module EqSat.TermIndex.SubstitutionTree -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow          (first, second)
import           Control.Exception
import           Control.Monad          (guard)
import           Data.Coerce            (coerce)
import           Data.Maybe
import           Data.Void              (Void, absurd)

-- import qualified Data.Vector.Primitive         as PV
-- import qualified Data.Vector.Primitive.Mutable as PMV

import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as Vector

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Flow

import           EqSat.Term             (TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term             as TTerm

import           EqSat.Internal.Refined (Refined, refine, unsafeRefine)
import qualified EqSat.Internal.Refined as Refined

--------------------------------------------------------------------------------

type Substitution node var = Map var (TTerm node var)

domain
  :: Substitution node var
  -> Set var
domain = Map.keysSet

codomain
  :: (Ord node, Ord var)
  => Substitution node var
  -> Set (TTerm node var)
codomain = Map.toList .> map snd .> Set.fromList

introduced
  :: (Ord var)
  => Substitution node var
  -> Set var
introduced = Map.toList .> map (snd .> TTerm.freeVars) .> mconcat

substitute
  :: (Ord var)
  => Substitution node var
  -> TTerm node var
  -> TTerm node var
substitute s (MkVarTerm var)
  = case Map.lookup var s of
      Nothing     -> MkVarTerm var
      (Just term) -> term
substitute s (MkNodeTerm n children)
  = MkNodeTerm n (Vector.map (substitute s) children)

-- | The identity substitution.
identity
  :: Substitution node var
identity = Map.empty

-- |
-- Composition of substitutions (in the same order as the @'<.'@ operator).
--
-- Laws:
--
-- 1. For any @s ∷ 'Substitution'@, @s ≡ 'composition' s 'identity'@.
-- 2. For any @s ∷ 'Substitution'@, @s ≡ 'composition' 'identity' s@.
-- 3. For any @a, b, c ∷ 'Substitution'@,
--    @'composition' ('composition' a b) c ≡ 'composition' a ('composition' b c)@.
composition
  :: (Ord var)
  => Substitution node var
  -> Substitution node var
  -> Substitution node var
composition s1 s2
  = Map.unionWith (\_ _ -> error "lol")
    (Map.map (substitute s1) s2)
    (Map.difference s1 s2)

-- |
-- The join of two substitutions, as defined in /Substitution Tree Indexing/
-- by Peter Graf.
--
-- Laws:
--
-- 1. FIXME: laws
join
  :: (Ord var)
  => Substitution node var
  -> Substitution node var
  -> Substitution node var
join s1 s2
  = Map.unionWith (\_ _ -> error "rofl")
    (Map.map (substitute s1) s2)
    ((domain s1 `Set.difference` introduced s2)
      |> Set.toList
      |> map (\k -> (k, fromJust (Map.lookup k s1)))
      |> Map.fromList)

--------------------------------------------------------------------------------

newtype Position
  = Position { fromPosition :: [Int] }
  deriving (Eq, Ord)

instance Show Position where
  show (Position xs) = show xs

(≺) :: Position -> Position -> Bool
(≺) p q = let go :: [Int] -> [Int] -> Bool
              go []         [] = False
              go []      (_:_) = False
              go (_:_)      [] = False
              go (p:ps) (q:qs) = (p < q) || ((p == q) && go ps qs)
          in go (fromPosition p) (fromPosition q)

consPosition :: Int -> Position -> Position
consPosition = coerce ((:) :: Int -> [Int] -> [Int])

indexPosition :: Position -> TTerm node var -> Maybe (TTerm node var)
indexPosition (Position [])         t = pure t
indexPosition (Position (i : rest)) t = do
  (MkNodeTerm node children) <- pure t
  guard (i >= 0)
  guard (i < Vector.length children)
  indexPosition (Position rest) (children ! i)

recursiveSubterms
  :: (Ord node, Ord var)
  => TTerm node var
  -> Map Position (TTerm node var)
recursiveSubterms (MkVarTerm var)
  = [(Position [], MkVarTerm var)]
recursiveSubterms (MkNodeTerm node children)
  = Vector.toList children
    |> zip [0 ..]
    |> map (second recursiveSubterms)
    |> map (\(i, m) -> Map.mapKeys (consPosition i) m)
    |> ([(Position [], MkNodeTerm node children)] :)
    |> Map.unionsWith (\_ _ -> error "lmao")

mapSubterms
  :: (Ord node, Ord var, Monad m)
  => TTerm node var
  -> (Position -> TTerm node var -> m ())
  -> m ()
mapSubterms term callback
  = undefined

traverseSubterms
  :: (Ord node, Ord var, Monad m)
  => TTerm node var
  -> (Position -> TTerm node var -> m ())
  -> m ()
traverseSubterms term callback
  = recursiveSubterms term
    |> Map.toList
    |> mapM_ (uncurry callback)

normalizeTerm
  :: TTerm node var
  -> (TTerm node Int, Vector var)
normalizeTerm = undefined

--------------------------------------------------------------------------------

newtype Indicator
  = Indicator { fromIndicator :: Int }
  deriving (Eq, Ord, Show)

data SubstitutionTree node var
  = SubstitutionTree
    { substitutionTreeContent  :: !(Substitution node (Either Indicator var))
    , substitutionTreeChildren :: !(Vector (SubstitutionTree node var))
    }
  deriving (Show)

insertTerm
  :: (Ord node, Ord var)
  => TTerm node var
  -> SubstitutionTree node var
  -> SubstitutionTree node var
insertTerm = undefined

lookupTerm
  :: (Ord node, Ord var)
  => SubstitutionTree node var
  -> TTerm node var
  -> Set ()
lookupTerm = undefined

--------------------------------------------------------------------------------

-- FIXME: move to tests

compositionTest :: IO ()
compositionTest = do
  let a, b :: Substitution String String
      a = [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("x", MkNodeTerm "a" [])
                 , ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ]
  let actual   = composition a b
  assert (expected == actual) (putStrLn "SUCCESS")

joinTest :: IO ()
joinTest = do
  let a, b :: Substitution String String
      a = [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ]
  let actual   = join a b
  assert (expected == actual) (putStrLn "SUCCESS")

recursiveSubtermsTest :: IO ()
recursiveSubtermsTest = do
  let a :: TTerm String String
      a = MkNodeTerm "h"
          [ MkNodeTerm "a" []
          , MkNodeTerm "g" [MkNodeTerm "b" []]
          , MkVarTerm "x"
          ]
  let expected = [ (Position [],     a)
                 , (Position [0],    MkNodeTerm "a" [])
                 , (Position [1],    MkNodeTerm "g" [MkNodeTerm "b" []])
                 , (Position [1, 0], MkNodeTerm "b" [])
                 , (Position [2],    MkVarTerm "x")
                 ]
  let actual   = recursiveSubterms a
  assert (expected == actual) (putStrLn "SUCCESS")
  assert
    (Map.toList expected
     |> all (\(p, st) -> Just st == indexPosition p a))
    (putStrLn "SUCCESS")

--------------------------------------------------------------------------------
