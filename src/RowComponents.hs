{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module RowComponents
  ( noDeps,
    WithDeps (..),
    withDeps,
    withDep,
  )
where

import Data.Kind (Type)
import Data.Row.Internal (LT ((:->)), Row (R))
import Data.Row.Records (Empty, Rec, type (.+), type (.==))
import GHC.TypeLits (Symbol)

-- type family Matches (spec :: Row Type) (r :: Row Type) :: Constraint where
--   Matches Empty r = ()
--   Matches ('R ((label ':-> t) ': pairs)) r = ((r .! label) ~ t, Matches ('R pairs) r)

-- class ((row .! label) ~ (Rec item), Matches spec item) => HasField row label spec item

-- instance ((row .! label) ~ (Rec item), Matches spec item) => HasField row label spec item

data BuildSpec (providedDeps :: Row Symbol) (requiredDeps :: Row Type) (a :: Type) where
  NoDeps :: (Rec requiredDeps -> IO a) -> BuildSpec Empty requiredDeps a
  ProvidingDep ::
    forall fromName asName provided required a.
    BuildSpec provided required a ->
    BuildSpec (provided .+ fromName .== asName) required a

{-# INLINE noDeps #-}
noDeps :: IO a -> BuildSpec Empty Empty a
noDeps build = NoDeps (const build)

type family ZipToRow (labels :: [Symbol]) (values :: [k]) :: Row k where
  ZipToRow '[] _ = Empty
  ZipToRow _ '[] = Empty
  ZipToRow (l ': ls) (v ': vs) = (l .== v) .+ ZipToRow ls vs

class WithDeps (deps :: Row Symbol) (spec :: Type) (spec' :: Type) | spec deps -> spec' where
  withRenamedDeps :: spec -> spec'

withDeps ::
  forall (deps :: [Symbol]) spec spec'.
  WithDeps (ZipToRow deps deps) spec spec' =>
  spec ->
  spec'
{-# INLINE withDeps #-}
withDeps = withRenamedDeps @(ZipToRow deps deps)

withDep ::
  forall (dep :: Symbol) spec spec'.
  WithDeps (dep .== dep) spec spec' =>
  spec ->
  spec'
{-# INLINE withDep #-}
withDep = withRenamedDeps @(dep .== dep)

instance
  ( WithDeps
      deps
      (BuildSpec Empty requiredDeps a)
      (BuildSpec providedDeps requiredDeps a)
  ) =>
  WithDeps
    deps
    (Rec requiredDeps -> IO a)
    (BuildSpec providedDeps requiredDeps a)
  where
  {-# INLINE withRenamedDeps #-}
  withRenamedDeps buildFn = (NoDeps buildFn) & withRenamedDeps @deps

instance
  (providedDeps ~ providedDeps') =>
  WithDeps
    Empty
    (BuildSpec providedDeps requiredDeps a)
    (BuildSpec providedDeps' requiredDeps a)
  where
  {-# INLINE withRenamedDeps #-}
  withRenamedDeps buildSpec = buildSpec

instance
  ( WithDeps
      ('R pairs)
      (BuildSpec (providedDeps .+ l .== v) requiredDeps a)
      (BuildSpec providedDeps' requiredDeps a)
  ) =>
  WithDeps
    ('R ((l ':-> v) ': pairs))
    (BuildSpec providedDeps requiredDeps a)
    (BuildSpec providedDeps' requiredDeps a)
  where
  {-# INLINE withRenamedDeps #-}
  withRenamedDeps buildFn = (ProvidingDep @l @v buildFn) & withRenamedDeps @('R pairs)
