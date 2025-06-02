{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Dictionaries where

import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (List1, NonEmpty ((:|)), (<|))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait
import Noll.Language.Type
import Noll.Language.Type.Kind
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

collectTraitsY ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Type TypeIndex Kind ->
  Name ->
  m [Trait (Type TypeIndex Kind)]
collectTraitsY u name = do
  env <- ask
  case Environment.lookup name env of
    Nothing ->
      pure []
    Just (Forall vs ts t) -> do
      undefined

traceType t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Traceable")
    (t :| [])

yy :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
yy =
  Environment.fromList
    [
      ( "trace"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Traceable" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
      )
    ,
      ( "pair_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          , Trait "Traceable" (TVariable (TypeIndex KType 1))
          ]
          ( traceType (TVariable (TypeIndex KType 0))
              `TArrow` traceType (TVariable (TypeIndex KType 1))
              `TArrow` TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
              `TArrow` TIntrinsic IString
          )
      )
    ,
      ( "list_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          ]
          ( traceType (TVariable (TypeIndex KType 0))
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
              `TArrow` TIntrinsic IString
          )
      )
    ]

-- zz :: Map IndexedType (Map String (Function Expression () IndexedType))
-- zz =
--  Map.fromList
--    [
--      ( TIntrinsic IString
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IString) "s") :| [])
--                (EVariable () (Label (TIntrinsic IString) "s"))
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic IInt32
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "int32_to_string"))
--                    (EVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    , Trait "Traceable" (TVariable (TypeIndex KType 1))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                )
--            )
--          ]
--      )
--    ]

xx :: Map IndexedType (Map String (Scheme TypeIndex Kind IndexedType))
xx =
  Map.fromList
    [
      ( TIntrinsic IString
      , Map.fromList
          [
            ( "trace"
            , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString)
            )
          ]
      )
    ,
      ( TIntrinsic IInt32
      , Map.fromList
          [
            ( "trace"
            , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString)
            )
          ]
      )
    ,
      ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
      , Map.fromList
          [
            ( "trace"
            , Forall
                (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                , Trait "Traceable" (TVariable (TypeIndex KType 1))
                ]
                (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
            )
          ]
      )
    ,
      ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
      , Map.fromList
          [
            ( "trace"
            , Forall
                (Set.fromList [TypeIndex KType 0])
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                ]
                (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
            )
          ]
      )
    ]

listPair =
  TApplication
    KType
    (TConstructor KType "list")
    ( TApplication
        KType
        (TConstructor (KType `KArrow` KType `KArrow` KType) "pair")
        ( TVariable (TypeIndex KType 0)
            <| TVariable (TypeIndex KType 1)
            :| []
        )
        :| []
    )

orderedDict t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Ordered")
    (t :| [])

sample1 =
  EApplication
    ()
    (TConstructor KType "Ordering")
    ( EVariable
        ()
        (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare")
    )
    ( EVariable () (Label listPair "xs")
        <| EVariable () (Label listPair "ys")
        :| []
    )

sample1Result1 =
  EApplication
    ()
    (TConstructor KType "Ordering")
    -- (EVariable () (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare"))
    ( EApplication
        ()
        undefined
        (EVariable () (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare"))
        (EDictionary () (orderedDict listPair) (Trait "Ordered" listPair) :| [])
    )
    ( EVariable () (Label listPair "xs")
        <| EVariable () (Label listPair "ys")
        :| []
    )

-- translator :: function -> args -> [traits] -> ([traits], expr)

-- appl1 = translator (compare fun) [xs, ys] [Ordered (pair(a, b))]
