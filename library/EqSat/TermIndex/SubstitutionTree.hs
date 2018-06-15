--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
import           Data.List              (sortBy)
import           Data.Maybe
import           Data.Ord               (comparing)
import           Data.Void              (Void, absurd)

-- import qualified Data.Vector.Primitive         as PV
-- import qualified Data.Vector.Primitive.Mutable as PMV

import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as Vector

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Flow                   ((.>), (|>))

import           EqSat.Term             (TTerm, Term (MkNodeTerm, MkVarTerm))
import qualified EqSat.Term             as TTerm

import           EqSat.Internal.Refined (Refined, refine, unsafeRefine)
import qualified EqSat.Internal.Refined as Refined

--------------------------------------------------------------------------------

newtype Substitution node v1 v2
  = Substitution
    { fromSubstitution :: Map v1 (TTerm node v2) }
  deriving (Eq, Show)

type Substitution' node var = Substitution node var var

domain
  :: Substitution node v1 v2
  -> Set v1
domain = fromSubstitution .> Map.keysSet

codomain
  :: (Ord node, Ord v1, Ord v2)
  => Substitution node v1 v2
  -> Set (TTerm node v2)
codomain = fromSubstitution .> Map.toList .> map snd .> Set.fromList

introduced
  :: (Ord v2)
  => Substitution node v1 v2
  -> Set v2
introduced = fromSubstitution
             .> Map.toList
             .> map (snd .> TTerm.freeVars)
             .> mconcat

substitute
  :: (Ord v1)
  => Substitution node v1 v2
  -> (v1 -> v2)
  -> TTerm node v1
  -> TTerm node v2
substitute (Substitution s) def = go
  where
    go (MkVarTerm var)   = Map.lookup var s
                           |> fromMaybe (MkVarTerm (def var))
    go (MkNodeTerm n cs) = MkNodeTerm n (Vector.map go cs)

substitute'
  :: (Ord var)
  => Substitution' node var
  -> TTerm node var
  -> TTerm node var
substitute' s term = substitute s id term

-- | The identity substitution.
identity
  :: Substitution' node var
identity = Substitution Map.empty

-- |
-- Composition of substitutions (in the same order as the @'<.'@ operator).
--
-- Laws:
--
-- 1. For any @s ∷ 'Substitution'@, @s ≡ 'compose' s 'identity'@.
-- 2. For any @s ∷ 'Substitution'@, @s ≡ 'compose' 'identity' s@.
-- 3. For any @a, b, c ∷ 'Substitution'@,
--    @'compose' ('compose' a b) c ≡ 'compose' a ('compose' b c)@.
compose
  :: (Ord var)
  => Substitution' node var
  -> Substitution' node var
  -> Substitution' node var
compose (Substitution s1) (Substitution s2)
  = Substitution (Map.unionWith (\_ _ -> error "lol")
                  (Map.map (substitute' (Substitution s1)) s2)
                  (Map.difference s1 s2))

-- |
-- The join of two substitutions, as defined in /Substitution Tree Indexing/
-- by Peter Graf.
--
-- Laws:
--
-- 1. FIXME: laws
join
  :: (Ord var)
  => Substitution' node var
  -> Substitution' node var
  -> Substitution' node var
join (Substitution s1) (Substitution s2)
  = (Map.unionWith (\_ _ -> error "rofl")
     (Map.map (substitute' (Substitution s1)) s2)
     ((domain (Substitution s1) `Set.difference` introduced (Substitution s2))
       |> Set.toList
       |> map (\k -> (k, fromJust (Map.lookup k s1)))
       |> Map.fromList))
    |> Substitution

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

firstOccurrences
  :: (Ord var)
  => TTerm node var
  -> Map var Position
firstOccurrences (MkVarTerm var)
  = [(var, Position [])]
firstOccurrences (MkNodeTerm node children)
  = Vector.toList children
    |> zip [0 ..]
    |> map (second firstOccurrences)
    |> map (\(i, m) -> Map.map (consPosition i) m)
    |> Map.unionsWith min

normalizeTerm
  :: forall node var.
     (Ord var)
  => TTerm node var
  -> (TTerm node Int, Vector var)
normalizeTerm term = (result, Vector.fromList sorted)
  where
    firsts :: Map var Position
    firsts = firstOccurrences term

    sorted :: [var]
    sorted = firsts
             |> Map.toList
             |> sortBy (\x y -> if snd x == snd y
                                then EQ
                                else (if snd x ≺ snd y then LT else GT))
             |> map fst

    substitution :: Substitution node var Int
    substitution = sorted
                   |> zip [0 ..]
                   |> map (\(i, v) -> (v, MkVarTerm i))
                   |> Map.fromList
                   |> Substitution

    result :: TTerm node Int
    result = substitute
             substitution
             (\_ -> error "normalizeTerm: this should never happen")
             term

normalizeSubstitution
  :: forall node var.
     (Ord var)
  => Substitution' node var
  -> (Substitution node var Int, Vector var)
normalizeSubstitution subst = (temp4, temp3)
  where
    temp1 :: Vector (var, TTerm (Maybe node) var)
    temp1 = fromSubstitution subst
            |> Map.map (TTerm.mapNode Just)
            |> Map.toList
            |> sortBy (comparing fst)
            |> Vector.fromList

    temp2 :: TTerm (Maybe node) Int
    temp3 :: Vector var
    (temp2, temp3) = Vector.map snd temp1
                     |> MkNodeTerm Nothing
                     |> normalizeTerm

    temp4 :: Substitution node var Int
    temp4 = [0 .. Vector.length temp1 - 1]
            |> map (\i -> indexPosition (Position [i]) temp2
                          |> fromMaybe (error "normalizeSubstitution: bad")
                          |> TTerm.mapNode fromJust)
            |> zip (map fst (Vector.toList temp1))
            |> Map.fromList
            |> Substitution

--------------------------------------------------------------------------------

newtype Indicator
  = Indicator { fromIndicator :: Int }
  deriving (Eq, Ord, Show)

data SubstitutionTree node var
  = SubstitutionTree
    { substitutionTreeContent  :: !(Substitution' node (Either Indicator var))
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

precTest :: IO ()
precTest = do
  assert (Position [1, 1] ≺ Position [1, 2])    (pure ())
  assert (Position [1, 1] ≺ Position [2])       (pure ())
  assert (Position [1, 2] ≺ Position [1, 2, 1]) (pure ())

composeTest :: IO ()
composeTest = do
  let a, b :: Substitution' String String
      a = Substitution [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = Substitution [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("x", MkNodeTerm "a" [])
                 , ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ] |> Substitution
  let actual   = compose a b
  assert (expected == actual) (putStrLn "SUCCESS")

joinTest :: IO ()
joinTest = do
  let a, b :: Substitution' String String
      a = Substitution [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = Substitution [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ] |> Substitution
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
