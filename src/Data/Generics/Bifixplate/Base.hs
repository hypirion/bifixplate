{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Generics.Bifixplate.Base where

import Data.Generics.Fixplate
import Data.Bifunctor

mapFirst :: (Bifunctor p, Functor (p a)) => (a -> b) -> Mu (p a) -> Mu (p b)
mapFirst f = cata (Fix . first f)

annMapFirst :: (Bifunctor p, Functor (p a)) =>
               (a -> b) -> Mu (Ann (p a) t) -> Mu (Ann (p b) t)
annMapFirst f = cata (Fix . go)
  where go (Ann attr' t) = Ann attr' (first f t)

