{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Kind.InferenceSpec where

import Coal.Language
import Coal.Language.Type
import Control.Monad.Identity (runIdentity)
import qualified Data.Set as Set

-- testType1 =
--  ( TArrow
--      (TVariable (Parameter () "a"))
--      (TVariable (Parameter () "b"))
--  )
--    `TArrow` TApplication () (TVariable (Parameter () "f")) (TVariable (Parameter () "a"))
--    `TArrow` TApplication () (TVariable (Parameter () "f")) (TVariable (Parameter () "b"))

-- testKinds1 =
--  runEmitConstraints mempty testType1
--
-- runTestKinds1 =
--  runIdentity $ do
--    let (t1, cs) = testKinds1
--    sub <- solveKindConstraints cs
--    pure (lowerKinds (applyKinds sub t1))
--
-- testTraitDefinition =
--  TraitDefinition
--    []
--    (Parameter () "m")
--    [
--      ( "bind"
--      , Forall
--          (Set.fromList [Parameter () "m", Parameter () "a", Parameter () "b"])
--          []
--          ( TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "a")) -- m<a>
--              ~> (TVariable (Parameter () "a") ~> TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "b"))) -- a -> m<b>
--              ~> TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "b")) -- m<b>
--          )
--      )
--    ]

--testTraitDefinition1 =
--  inferTraitKinds
--    mempty
--    ( TraitDefinition
--        []
--        (Parameter () "m")
--        [
--          ( "bind"
--          , Forall
--              (Set.fromList [Parameter () "m", Parameter () "a", Parameter () "b"])
--                mempty
--              ( TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "a")) -- m<a>
--                  ~> (TVariable (Parameter () "a") ~> TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "b"))) -- a -> m<b>
--                  ~> TApplication () (TVariable (Parameter () "m")) (TVariable (Parameter () "b")) -- m<b>
--              )
--          )
--        ]
--    )
--    == Right
--      ( TraitDefinition
--          []
--          (Parameter (KArrow KType KType) "m")
--          [
--            ( "bind"
--            , Forall
--                (Set.fromList [Parameter (KArrow KType KType) "m", Parameter KType "a", Parameter KType "b"])
--                mempty
--                ( TApplication KType (TVariable (Parameter (KArrow KType KType) "m")) (TVariable (Parameter KType "a")) -- m<a>
--                    ~> (TVariable (Parameter KType "a") ~> TApplication KType (TVariable (Parameter (KArrow KType KType) "m")) (TVariable (Parameter KType "b"))) -- a -> m<b>
--                    ~> TApplication KType (TVariable (Parameter (KArrow KType KType) "m")) (TVariable (Parameter KType "b")) -- m<b>
--                )
--            )
--          ]
--      )
