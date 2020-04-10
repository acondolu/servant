{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines the error messages used in type-level errors.
-- Type-level errors can signal non-existing instances, for instance when
-- a combinator is not applied to the correct number of arguments.

module Servant.API.TypeErrors (
  NoInstanceFor,
  PartialApplication
) where

import  GHC.TypeLits
        (ErrorMessage(..), Symbol)


-- | No instance exists for @tclass (expr :> ...)@ because 
-- @expr@ is not recognised.
type NoInstanceFor (tclass :: k) (expr :: k') =
  Text "There is no instance for " :<>: ShowType tclass
  :<>: Text " (" :<>: ShowType expr :<>: Text " :> ...)"

-- | No instance exists for @tclass (expr :> ...)@ because @expr@ is not fully saturated.
-- @head@ is the head of @expr@, and it requires a number of arguments equals to @arity@.
type PartialApplication (tclass :: k) (expr :: k') (head :: k'') (arity :: Symbol) =
  NoInstanceFor tclass expr
  :$$: ShowType head :<>: Text " expects " :<>: Text arity :<>: Text " arguments"
