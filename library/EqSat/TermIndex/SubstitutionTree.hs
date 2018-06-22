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
import           Data.Either
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

import           EqSat.Substitution
                 (EndoSubstitution,
                 Substitution (Substitution, fromSubstitution),
                 TotalSubstitution (TotalSubstitution))
import qualified EqSat.Substitution     as Substitution

import           EqSat.Internal.Refined (Refined, refine, unsafeRefine)
import qualified EqSat.Internal.Refined as Refined

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
    result = Substitution.substituteTotal
             (TotalSubstitution
              substitution
              (\_ -> error "normalizeTerm: this should never happen"))
             term

normalizeSubstitution
  :: forall node var.
     (Ord var)
  => EndoSubstitution node var
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

-- | An antiunification algorithm on terms of type @'TTerm' node var@, as
--   defined in section 7.3 of /Substitution Tree Indexing/.
newtype Generalizer node var
  = Generalizer
    { runGeneralizer :: EndoSubstitution node var
                     -> EndoSubstitution node var
                     -> ( (EndoSubstitution node var, EndoSubstitution node var)
                        , EndoSubstitution node var
                        )
    }

computeMSCG
  :: (Eq node, Ord var)
  => Generalizer node var
  -> EndoSubstitution node var
  -> EndoSubstitution node var
  -> ( (EndoSubstitution node var, EndoSubstitution node var)
     , EndoSubstitution node var
     )
computeMSCG (Generalizer f) s₁ s₂
  = let ((σ₁, σ₂), μ) = f s₁ s₂
    in (    ((σ₁ `Substitution.join` μ) == s₁)
         && ((σ₂ `Substitution.join` μ) == s₂)
       ) `assert` ((σ₁, σ₂), μ)

--------------------------------------------------------------------------------

newtype Indicator
  = Indicator { fromIndicator :: Int }
  deriving (Eq, Ord, Show)

data SubstitutionTree node var
  = SubstitutionTree
    { substitutionTreeContent  :: !(EndoSubstitution node (Either Indicator var))
    , substitutionTreeChildren :: !(Vector (SubstitutionTree node var))
    }
  deriving (Show)

createNode
  :: forall node var.
     (Ord var)
  => EndoSubstitution node (Either Indicator var)
  -> Vector (SubstitutionTree node var)
  -> Maybe (SubstitutionTree node var)
createNode = (\content children -> if validate content children
                                   then Just (SubstitutionTree content children)
                                   else Nothing)
  where
    validate
      :: EndoSubstitution node (Either Indicator var)
      -> Vector (SubstitutionTree node var)
      -> Bool
    validate content children
      = (Vector.length children /= 1)
        && go1 undefined {- FIXME -} (SubstitutionTree content children)
        && go2 Set.empty (SubstitutionTree content children)

    -- For every path (Σ₁, Ω₁), …, (Σₙ, Ωₙ) from the root to a leaf of a
    -- non-empty tree, I(Σₙ • … • Σ₁) ⊂ V*.
    go1 :: EndoSubstitution node (Either Indicator var)
        -> SubstitutionTree node var
        -> Bool
    go1 s (SubstitutionTree content children)
      = let s' = content `Substitution.join` s
        in if Vector.null children
           then all isLeft (Substitution.introduced s')
           else Vector.all (go1 s') children

    -- For every path (Σ₁, Ω₁), …, (Σₙ, Ωₙ) from the root to a leaf of a
    -- non-empty tree, DOM(Σᵢ) ∩ (DOM(Σ₁) ∪ … ∪ DOM(Σᵢ₋₁)) = ∅
    go2 :: Set (Either Indicator var)
        -> SubstitutionTree node var
        -> Bool
    go2 vs (SubstitutionTree content children)
      = let d = Substitution.domain content
        in Set.null (vs `Set.intersection` d)
           && Vector.all (go2 (vs `Set.union` d)) children

allSubstitutions
  :: (Ord var, Ord node)
  => SubstitutionTree node var
  -> Set (EndoSubstitution node (Either Indicator var))
allSubstitutions (SubstitutionTree content children)
  = Set.singleton content
    `Set.union`
    (mconcat (map allSubstitutions (Vector.toList children)))

insertSubstitution
  :: (Ord node, Ord var)
  => EndoSubstitution node var
  -> SubstitutionTree node var
  -> SubstitutionTree node var
insertSubstitution = undefined

lookupSubstitution
  :: (Ord node, Ord var)
  => SubstitutionTree node var
  -> EndoSubstitution node var
  -> Set (EndoSubstitution node var)
lookupSubstitution = undefined

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
  -> Set (TTerm node var)
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
  let a, b :: EndoSubstitution String String
      a = Substitution [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = Substitution [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("x", MkNodeTerm "a" [])
                 , ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ] |> Substitution
  let actual   = Substitution.composeEndo a b
  assert (expected == actual) (putStrLn "SUCCESS")

joinTest :: IO ()
joinTest = do
  let a, b :: EndoSubstitution String String
      a = Substitution [("x", MkNodeTerm "a" []), ("y", MkNodeTerm "c" [])]
      b = Substitution [("z", MkNodeTerm "f" [MkVarTerm "x"])]
  let expected = [ ("y", MkNodeTerm "c" [])
                 , ("z", MkNodeTerm "f" [MkNodeTerm "a" []])
                 ] |> Substitution
  let actual   = Substitution.join a b
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
