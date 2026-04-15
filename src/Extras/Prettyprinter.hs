module Extras.Prettyprinter (
  -- * Precedence-based printing
  Prec,
  precArrow,
  precApp,
  precAtom,
  parensIf,

  -- * Bracket helpers
  typeBrackets,
) where

import Prettyprinter (Doc, Pretty (..), encloseSep, parens)

{- | Precedence level for controlling parenthesization.

Precedence levels determine when parentheses are needed around sub-expressions.
The system follows these principles:

__How Precedence Works:__

1. __Lower numbers = looser binding__ (needs parentheses more often)
2. __Higher numbers = tighter binding__ (needs parentheses less often)
3. __Rule:__ When printing at precedence level @P@, sub-expressions with
 precedence @< P@ need parentheses

__Precedence Levels:__

* @precArrow = 1@: Function arrows and similar binary operators (loosest)
* @precApp = 2@: Function/type application (medium)
* @precAtom = 3@: Atomic expressions - variables, literals, parens (tightest)

__Example: Function Types__

Consider printing @(a -> b) -> c@:

1. The outer arrow prints at @precArrow = 1@
2. The left operand @a -> b@ also has precedence @precArrow = 1@
3. Since @1 >= 1@, we need parentheses: @parensIf (1 > 1) = False@ (wait, this is wrong!)

Actually, the pattern is:

@
prettyTypePrec prec (TArrow t1 t2) =
  parensIf (prec > precArrow) $
      prettyTypePrec (precArrow + 1) t1 <+> "->" <+> prettyTypePrec precArrow t2
@

This means:

1. Outer context checks: "Do I need parens?" → Only if @prec > 1@ (e.g., inside an application)
2. Print left side at @precArrow + 1 = 2@, so nested arrows need parens: @(a -> b)@
3. Print right side at @precArrow = 1@, so nested arrows don't: @b -> c@
4. Result: @(a -> b) -> c@ (left gets parens, right doesn't - right associative!)

__Example: Type Application__

For @List<a -> b>@:

@
prettyAppPrec prec con args =
  parensIf (prec > precApp) $
      prettyTypePrec precApp con <> brackets (map (prettyTypePrec 0) args)
@

1. Arguments print at precedence @0@ (outermost), so @a -> b@ needs no parens... wait, that's not right!

Let me reconsider. Looking at the actual code:

@
prettyTypeApplicationPrec prec con args =
parensIf (prec > precApp) $
  group (prettyTypePrec precApp con <> typeBrackets (map (prettyTypePrec 0) (toList args)))
@

Arguments are printed at precedence 0 (outermost context), so they get minimal parenthesization.
The whole application gets parentheses only if the surrounding context has @prec > precApp@.

__Key Insight:__

* @parensIf (prec > P)@ means "add parens if I'm in a context tighter than my own precedence"
* When recursing, use @P + 1@ for operands that shouldn't be the same operator (forces parens on same-level ops)
* When recursing, use @0@ or @P@ for operands where you want minimal parens

__Summary Rules:__

1. __Check for parens:__ @parensIf (currentPrec > myPrecedence)@
2. __Left-assoc operators:__ Print left at @myPrec + 1@, right at @myPrec@
3. __Right-assoc operators:__ Print left at @myPrec + 1@, right at @myPrec@ (e.g., arrows)
4. __Arguments/isolated contexts:__ Print at @0@ for minimal parens
5. __Atoms:__ Never need parens (highest precedence)
-}
type Prec = Int

{- | Precedence for function arrows and similar operators (loosest binding)

Used for: @a -> b@, @a => b@, and similar binary operators

Example: @a -> b -> c@ prints without parentheses
        @(a -> b) -> c@ requires parentheses on the left
-}
precArrow :: Prec
precArrow = 1

{- | Precedence for function/type application (medium binding)

Used for: @f x@, @List\<Int\>@, @T a b@

Example: @f (a -> b)@ requires parentheses around the arrow
        @f a b@ prints without parentheses
-}
precApp :: Prec
precApp = 2

{- | Precedence for atomic expressions (tightest binding, never needs parens)

Used for: variables, literals, constructors, parenthesized expressions

Example: @x@, @42@, @"hello"@, @(a + b)@ never need additional parentheses
-}
precAtom :: Prec
precAtom = 3

{- | Conditionally wrap output in parentheses based on a boolean condition

@parensIf True doc@ wraps @doc@ in parentheses: @(doc)@
@parensIf False doc@ returns @doc@ unchanged

Typically used with precedence comparisons:

@
parensIf (currentPrec > myPrec) doc
@
-}
parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

{- | Format a list of documents as type arguments: @\<arg1, arg2, arg3\>@

Used for generic type application in Coal's syntax
-}
typeBrackets :: [Doc ann] -> Doc ann
typeBrackets = encloseSep (pretty '<') (pretty '>') (pretty ", ")
