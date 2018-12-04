{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module EqSat.Normalization where

import           Bound
import           Control.Monad        (ap)
import           Data.Deriving
                 (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import           Data.Functor.Classes
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Unique
import           Data.Void            (Void)
import           Flow

data Variable v
  = BVar {-# UNPACK #-} !Unique
  | FVar {-# UNPACK #-} !v
  deriving (Eq, Ord)

data HOType
  = Star
  | !HOType :-> !HOType
  deriving (Eq, Ord, Show, Read)

infixr 9 :->

data HOTerm v where
  Var   :: !v                           -> HOTerm v
  (:@:) :: !(HOTerm v) -> !(HOTerm v)   -> HOTerm v
  Lam   :: !HOType -> !v -> !(HOTerm v) -> HOTerm v

infixl 9 :@:

deriving instance (Eq   v) => Eq   (HOTerm v)
deriving instance (Show v) => Show (HOTerm v)
deriving instance Functor     HOTerm
deriving instance Foldable    HOTerm
deriving instance Traversable HOTerm

uniquify :: forall v. (Ord v) => HOTerm v -> IO (HOTerm (Variable v))
uniquify = go Map.empty
  where
    go :: Map v Unique -> HOTerm v -> IO (HOTerm (Variable v))
    go m (Var v) = undefined

containsBetaRedex :: HOTerm v -> Bool
containsBetaRedex (Lam _ _ _ :@: _) = True
containsBetaRedex (s :@: t)         = containsBetaRedex s || containsBetaRedex t
containsBetaRedex (Lam _ _ b)       = undefined
containsBetaRedex _                 = False

isInBNF :: HOTerm v -> Bool
isInBNF = containsBetaRedex .> not

betaReduce :: HOTerm v -> HOTerm v
betaReduce = undefined

-- betaReduce :: HOTerm v -> HOTerm v
-- betaReduce (Var v)   = Var v
-- betaReduce (Lam t b) = Lam t $ toScope $ betaReduce $ fromScope b
-- betaReduce (f :@: a) = case whnf f of
--                          (Lam _ b) -> betaReduce (instantiate1 a b)
--                          f'        -> betaReduce f' :@: betaReduce a

etaExpand :: (Eq v) => HOTerm v -> HOTerm v
etaExpand = fix (\t -> let t' = go t
                       in if isInBNF t' then t' else t)
  where
    go (Var v)     = Var v
    go (f :@: a)   = etaExpand f
    go (Lam t v b) = undefined

    fix :: (Eq a) => (a -> a) -> a -> a
    fix f x = let x' = f x
              in if x == x' then x else fix f x'

normalize :: (Eq v) => HOTerm v -> HOTerm v
normalize = betaReduce .> etaExpand

combinatorS, combinatorK, combinatorI :: IO (HOTerm (Variable Void))
combinatorS
  = Lam (Star :-> Star :-> Star) 'x'
    (Lam (Star :-> Star) 'y'
     (Lam Star 'z'
      (Var 'x' :@: Var 'z' :@: (Var 'y' :@: Var 'z'))))
    |> uniquify
    |> fmap (fmap (error ""))
combinatorK
  = lam Star 'x' (lam Star 'y' (Var 'x'))
    |> uniquify
combinatorI
  = lam Star 'x' (Var 'x')
    |> uniquify

churchNatural :: Int -> IO (HOTerm (Variable v))
churchNatural = uniquify
                . lam (Star :-> Star) 'f'
                . lam Star 'x'
                . go (Var 'x')
  where
    go t 0 = t
    go t n = go (Var 'f' :@: t) (n - 1)

churchPlus :: IO (HOTerm (Variable v))
churchPlus = lam (Star :-> Star :-> Star) 'm'
             (lam (Star :-> Star :-> Star) 'n'
              (lam Star 'f'
               (lam Star 'x'
                (Var 'm' :@: Var 'f' :@: (Var 'n' :@: Var 'f' :@: Var 'x')))))
             |> uniquify

allEqual :: (Eq a) => [a] -> Bool
allEqual []             = True
allEqual [_]            = True
allEqual (x : y : rest) = (x == y) && allEqual (y : rest)

data TestResult a
  = Success
  | Failure a
  deriving (Eq, Ord, Show, Read)

normalizeTest :: IO [TestResult [HOTerm (Variable Void)]]
normalizeTest = do
  let s = combinatorS
  let k = combinatorK
  let i = combinatorI
  id ( [ [ (s :@: k :@: k)
         , i
         ]
       , [ (s
            :@: (k :@: (s :@: i))
            :@: (s :@: (k :@: k) :@: i)
            :@: k
            :@: k)
         , (k :@: k)
         ]
       , [ (s
            :@: (k :@: (s :@: i))
            :@: (s :@: (k :@: k) :@: i))
         , (s :@: (k :@: (s :@: i)) :@: k)
         ]
       , [ churchPlus
           :@: churchNatural 15
           :@: churchNatural 10
         , churchNatural 25
         ]
       -- , [ churchPlus
       --     :@: churchNatural 1515
       --     :@: churchNatural 2525
       --   , churchNatural 4040
       --   ]
       ] |> (map (\xs -> let xs' = map betaReduce xs
                         in if allEqual xs' && isInBNF (head xs')
                            then Success
                            else Failure xs)
             .> _)
     )
