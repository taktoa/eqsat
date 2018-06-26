{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module EqSat.Normalization where

import           Data.Void (Void)
import           Flow

data HOTerm c where
  Var   :: !Int                       -> HOTerm c
  Const :: !c                         -> HOTerm c
  (:@:) :: !(HOTerm c) -> !(HOTerm c) -> HOTerm c
  Lam   :: !(HOTerm c)                -> HOTerm c

deriving instance (Eq   c) => Eq   (HOTerm c)
deriving instance (Ord  c) => Ord  (HOTerm c)
deriving instance (Show c) => Show (HOTerm c)
deriving instance (Read c) => Read (HOTerm c)

infixl 9 :@:

abstract :: HOTerm c -> HOTerm c
abstract = go 0
  where
    go n (Var i)   = Var (if i < n then i else i + 1)
    go n (Const c) = Const c
    go n (s :@: t) = go n s :@: go n t
    go n (Lam t)   = Lam (go (n + 1) t)

instantiate :: HOTerm c -> HOTerm c -> HOTerm c
instantiate repl = go 0
  where
    go n (Lam t)   = Lam (go (n + 1) t)
    go n (s :@: t) = go n s :@: go n t
    go n (Const c) = Const c
    go n (Var i)   = if i == n then repl else Var i

-- abstract :: HOTerm c -> HOTerm
-- abstract = go 0
--   where
--     go n (Var i)   = Var (if i < n then i + δ else i)
--     go n (Const c) = Const c
--     go n (s :@: t) = (shift δ s) :@: (shift δ t)
--     go n (Lam t)   = Lam (go (n + 1) t)

betaReduce :: forall c. HOTerm c -> HOTerm c
betaReduce = \case (Lam s :@: t) -> instantiate t s
                   other         -> other
  where
    go :: (Int, HOTerm c) -> HOTerm c -> HOTerm c
    go (var, term)
      = \case (Var n)   -> if n == var
                           then term
                           else Var n
              (Const c) -> Const c
              (s :@: t) -> let f = go (var, term)
                           in (f s) :@: (f t)
              (Lam t)   -> Lam (go (var, abstract term) t)

containsBetaRedex :: HOTerm c -> Bool
containsBetaRedex (Lam _ :@: _) = True
containsBetaRedex (s :@: t)     = [ containsBetaRedex s
                                  , containsBetaRedex t
                                  ] |> or
containsBetaRedex (Lam t)       = containsBetaRedex t
containsBetaRedex _             = False

etaExpand :: HOTerm c -> HOTerm c
etaExpand term = let result = Lam (term :@: Var 0)
                 in if containsBetaRedex result then term else result

completeBetaReduce :: HOTerm c -> HOTerm c
completeBetaReduce (Lam t)       = Lam (completeBetaReduce t)
completeBetaReduce (s :@: t)     = betaReduce (completeBetaReduce s :@: completeBetaReduce t)
completeBetaReduce other         = other

normalize :: forall c. (Eq c) => HOTerm c -> HOTerm c
normalize = fix completeBetaReduce
  where
    completeEtaExpand :: HOTerm c -> HOTerm c
    completeEtaExpand (Lam t)   = etaExpand (Lam (completeEtaExpand t))
    completeEtaExpand (s :@: t) = etaExpand
                                  (completeEtaExpand s :@: completeEtaExpand t)
    completeEtaExpand other     = etaExpand other

    fix :: (Eq a) => (a -> a) -> (a -> a)
    fix f x = let x' = f x
              in if x == x' then x else fix f x'

combinatorS, combinatorK, combinatorI :: HOTerm c
combinatorS = Lam (Lam (Lam (Var 2 :@: Var 0 :@: (Var 1 :@: Var 0))))
combinatorK = Lam (Lam (Var 1))
combinatorI = Lam (Var 0)

data TestResult a
  = Success
  | Failure (a, a)
  deriving (Eq, Ord, Show, Read)

normalizeTest :: [TestResult (HOTerm Void)]
normalizeTest = [ ( normalize (combinatorS :@: combinatorK :@: combinatorK)
                  , combinatorI
                  )
                ] |> map (\(x, y) -> if x == y then Success else Failure (x, y))


-- subst :: (Int, HOTerm c) -> HOTerm c -> HOTerm c
-- subst (var, replacement)
--   = \case (Var n)   -> if n == var
--                        then replacement
--                        else Var n
--           (Const c) -> Const c
--           (App s t) -> let f = subst (var, replacement)
--                        in App (f s) (f t)
--           (Lam t)   -> Lam (subst (var, replacement) t)
--
-- whnf :: forall c. HOTerm c -> HOTerm c
-- whnf = go 0
--   where
--     go :: Int -> HOTerm c -> HOTerm c
--     go n (App (Lam s) t) =
