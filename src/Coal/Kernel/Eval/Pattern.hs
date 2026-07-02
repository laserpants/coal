{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

{- |
Pattern matching for case expressions.

Implements runtime pattern matching against data constructor values. Tries
each clause in sequence until one matches, extracting bindings for pattern
variables.

= Matching rules

  * The first label in a clause is the constructor name
  * Subsequent labels are pattern variables (bindings for field values)
  * Constructor name must match the scrutinee's constructor
  * Arity (number of fields) must match the pattern's variable count

Fails with 'PatternMatchFailure' if no clause matches.
-}
module Coal.Kernel.Eval.Pattern (
  matchClause,
) where

import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import Coal.Kernel.Eval.State (EvalError (..), EvalM, throwEval)
import Coal.Kernel.Eval.Value (Value (..))
import Coal.Kernel.Language.Expr (Clause (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Common (Name)

{- | Try each clause in order against a scrutinee value.

Returns a list of (name, value) bindings to add to the environment, plus the
clause body, when a match is found. Fails with 'PatternMatchFailure' if no
clause matches.
-}
matchClause ::
  Value ->
  NonEmpty (Clause Type) ->
  EvalM ([(Name, Value)], Clause Type)
matchClause scrutinee clauses =
  case filter (canMatch scrutinee . clausePatterns) (NonEmpty.toList clauses) of
    [] ->
      throwEval
        ( PatternMatchFailure
            ( "No clause matched: "
                ++ describeValue scrutinee
                ++ " against clauses ["
                ++ intercalate ", " (map showClauseHead (NonEmpty.toList clauses))
                ++ "]"
            )
        )
    (clause : _) ->
      case extractBindings scrutinee (clausePatterns clause) of
        Nothing ->
          throwEval (PatternMatchFailure "Internal: binding extraction failed after successful match test")
        Just bindings ->
          return (bindings, clause)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

clausePatterns :: Clause Type -> NonEmpty (Label Type)
clausePatterns (Clause pats _) = pats

{- | A single-label clause: the label is just a variable name (always matches).
A multi-label clause: the first label is the constructor name; the rest are field binders.
Match succeeds when:
  - The scrutinee is the right constructor (name matches), OR
  - There is exactly one label and it is a plain variable binder (catch-all).

Constructor names start with an uppercase letter (e.g. "Main.Leaf").
Variable binders start with a lowercase letter (e.g. "x", "lhs").
-}
canMatch :: Value -> NonEmpty (Label Type) -> Bool
canMatch scrutinee pats =
  case NonEmpty.toList pats of
    [] -> False -- impossible by parser invariant
    [Label _ name] ->
      if isConstructorName name
        then -- Constructor pattern: only matches the named constructor.
        case scrutinee of
          VConstructor conName _ _ ->
            conName == name
          _ ->
            False
        else True -- Variable binder: always matches.
    (Label _ conName : _) ->
      -- Multi-label: first label must name the constructor.
      case scrutinee of
        VConstructor sname _ _ -> sname == conName
        _ -> False

-- | Constructor names start with an uppercase letter, optionally preceded by '$'.
isConstructorName :: Name -> Bool
isConstructorName name =
  let stripped = Text.dropWhile (== '$') name
   in not (Text.null stripped) && isUpper (Text.head stripped)

-- | Extract variable bindings from a successful pattern match.
extractBindings :: Value -> NonEmpty (Label Type) -> Maybe [(Name, Value)]
extractBindings scrutinee pats =
  case NonEmpty.toList pats of
    [] -> Nothing
    [Label _ name] ->
      if isConstructorName name
        then -- Constructor pattern: no field bindings; the name itself is not bound.
          Just []
        else -- Variable binder: bind the scrutinee to the name.
          Just [(name, scrutinee)]
    (_ : fieldPats) ->
      -- Multi-label: skip the constructor name label, bind field values to field labels.
      case scrutinee of
        VConstructor _ _ fields ->
          if length fieldPats == length fields
            then Just (zipWith (\(Label _ n) v -> (n, v)) fieldPats fields)
            else Nothing
        _ -> Nothing

describeValue :: Value -> String
describeValue = \case
  VUnit ->
    "unit"
  VBool b ->
    if b then "true" else "false"
  VInt32 n ->
    "int32:" ++ show n
  VInt64 n ->
    "int64:" ++ show n
  VBignum n ->
    "bignum:" ++ show n
  VFloat f ->
    "float:" ++ show f
  VDouble d ->
    "double:" ++ show d
  VChar c ->
    "char:" ++ show c
  VString _ ->
    "string"
  VConstructor name _ _ ->
    "constructor:" ++ show name
  VRecord _ ->
    "record"
  VClosure _ ->
    "closure"
  VExtern name ->
    "extern:" ++ show name

showClauseHead :: Clause Type -> String
showClauseHead (Clause pats _) =
  "(" ++ intercalate ", " (map (\(Label _ n) -> show n) (NonEmpty.toList pats)) ++ ")"
