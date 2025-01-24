{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Type.AliasInsertion where

import Control.Monad.Reader (MonadReader)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Language (
  Constant (..),
  Expression (..),
  Function (..),
  IndexedType,
  Module (..),
  Object (..),
  Pattern (..),
  Row (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.Utils (Dictionary, Name)

class AliasContext c where
  insertAliases :: (MonadReader (Environment ([Name], IndexedType)) m) => c -> m c

instance AliasContext () where
  insertAliases _ = pure ()

instance (AliasContext c) => AliasContext [c] where
  insertAliases = traverse insertAliases

instance (AliasContext c) => AliasContext (Dictionary c) where
  insertAliases = traverse insertAliases

instance (AliasContext c) => AliasContext (NonEmpty c) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Trait t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Uses t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Row o k t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Pattern a t) where
  insertAliases = undefined

instance (AliasContext t) => AliasContext (Expression a t) where
  insertAliases = undefined

instance (AliasContext t) => AliasContext (Module e a t) where
  insertAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Function e a t) where
  insertAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Constant e a t) where
  insertAliases = undefined

instance (AliasContext t) => AliasContext (Object a k t) where
  insertAliases = undefined

instance AliasContext (Type TypeIndex k) where
  insertAliases = undefined
