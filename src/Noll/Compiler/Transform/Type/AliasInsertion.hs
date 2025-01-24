{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Type.AliasInsertion where

import Control.Monad.Reader (MonadReader)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Language (
  Module (..),
  Constant (..),
  Expression (..),
  Pattern (..),
  Object (..),
  Function (..),
  IndexedType,
  Object (..),
  Row (..),
  Trait (..),
  Type (..),
  Uses (..),
 )
import Noll.Utils (Dictionary, Name)

class AliasContext c where
  expandAliases :: (MonadReader (Environment ([Name], IndexedType)) m) => c -> m c

instance AliasContext () where
  expandAliases _ = pure ()

instance (AliasContext c) => AliasContext [c] where
  expandAliases = traverse expandAliases

instance (AliasContext c) => AliasContext (Dictionary c) where
  expandAliases = traverse expandAliases

instance (AliasContext c) => AliasContext (NonEmpty c) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Trait t) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Uses t) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Row o k t) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Pattern a t) where
  expandAliases = undefined

instance (AliasContext t) => AliasContext (Expression a t) where
  expandAliases = undefined

instance (AliasContext t) => AliasContext (Module e a t) where
  expandAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Function e a t) where
  expandAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Constant e a t) where
  expandAliases = undefined

instance (AliasContext t) => AliasContext (Object a k t) where
  expandAliases = undefined
