module DhallErrors () where

import Dhall.TypeCheck


simpleTextMessage :: (Eq a) => TypeMessage s a -> Text
simpleTextMessage (UnboundVariable x) = "Unbound variable: " <> x
simpleTextMessage (InvalidInputType expr) = "Invalid function input"
simpleTextMessage (InvalidOutputType expr) = "Invalid function output"
simpleTextMessage (NotAFunction expr0 expr1) = "Not a function"
simpleTextMessage (TypeMismatch expr0 expr1 expr2 expr3) = ErrorMessages {..}
  wher
        <>  prettyDiff expr1 expr3

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2
        txt3 = insert expr3

simpleTextMessage (AnnotMismatch expr0 expr1 expr2) = ErrorMessages {..}
  wher
        <>  prettyDiff expr1 expr2
    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

simpleTextMessage Untyped = 
     "❰Sort❱ has no type, kind, or sort"

    

simpleTextMessage (InvalidPredicate expr0 expr1) = 
     "Invalid predicate for ❰if❱"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (IfBranchMustBeTerm b expr0 expr1 expr2) =
    
     "❰if❱ branch is not a term"

    
      where
        txt0 = if b then "then" else "else"
        txt1 = insert expr0
        txt2 = insert expr1
        txt3 = insert expr2

simpleTextMessage (IfBranchMismatch expr0 expr1 expr2 expr3) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr1 expr3

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2
        txt3 = insert expr3

simpleTextMessage (InvalidListType expr0) = 
     "Invalid type for ❰List❱ elements"

    
      where
        txt0 = insert expr0

simpleTextMessage MissingListType = do
    
     "An empty list requires a type annotation"

    

simpleTextMessage (MismatchedListElements i expr0 _expr1 expr2) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr0 expr2

    
      where
        txt0 = insert expr0
        txt1 = pretty i
        txt3 = insert expr2

simpleTextMessage (InvalidListElement i expr0 _expr1 expr2) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr0 expr2

    
      where
        txt0 = insert expr0
        txt1 = pretty i
        txt3 = insert expr2

simpleTextMessage (InvalidOptionalType expr0) = 
     "Invalid type for ❰Optional❱ element"

    
      where
        txt0 = insert expr0

simpleTextMessage (InvalidOptionalElement expr0 expr1 expr2) = ErrorMessages {..}
  wher
        <>  prettyDiff expr0 expr2

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

simpleTextMessage (InvalidSome expr0 expr1 expr2) = 
     "❰Some❱ argument has the wrong type"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

simpleTextMessage (InvalidFieldType k expr0) = 
     "Invalid field type"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (FieldAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = 
     "Field annotation mismatch"

    
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "❰Type❱"
        level Kind = "❰Kind❱"
        level Sort = "❰Sort❱"

simpleTextMessage (FieldMismatch k0 expr0 c0 k1 expr1 c1) = 
     "Field mismatch"

    
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "term"
        level Kind = "type"
        level Sort = "kind"

simpleTextMessage (InvalidField k expr0) = 
     "Invalid field"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (InvalidAlternativeType k expr0) = 
     "Invalid alternative type"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (InvalidAlternative k expr0) = 
     "Invalid alternative"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (AlternativeAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = 
     "Alternative annotation mismatch"

    
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "❰Type❱"
        level Kind = "❰Kind❱"
        level Sort = "❰Sort❱"

simpleTextMessage (ListAppendMismatch expr0 expr1) = ErrorMessages {..}
  wher
        <>  prettyDiff expr0 expr1

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (DuplicateAlternative k) = 
     "Duplicate union alternative"

    
      where
        txt0 = insert k

simpleTextMessage (MustCombineARecord c expr0 expr1) = 
     "You can only combine records"

    
      where
        op   = pretty c
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (RecordMismatch c expr0 expr1 const0 const1) = 
     "Record mismatch"

    
      where
        op   = pretty c

        txt0 = insert expr0
        txt1 = insert expr1

        toClass Type = "terms"
        toClass Kind = "types"
        toClass Sort = "kinds"

        class0 = toClass const0
        class1 = toClass const1

simpleTextMessage (CombineTypesRequiresRecordType expr0 expr1) =
    
     "❰⩓❱ requires arguments that are record types"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (RecordTypeMismatch const0 const1 expr0 expr1) =
    
     "Record type mismatch"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert const0
        txt3 = insert const1

simpleTextMessage (FieldCollision k) = 
     "Field collision"

    
      where
        txt0 = insert k

simpleTextMessage (MustMergeARecord expr0 expr1) = 
     "❰merge❱ expects a record of handlers"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (MustMergeUnion expr0 expr1) = 
     "❰merge❱ expects a union"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (UnusedHandler ks) = 
     "Unused handler"

    
      where
        txt0 = insert (Text.intercalate ", " (Data.Set.toList ks))

simpleTextMessage (MissingHandler ks) = 
     "Missing handler"

    
      where
        txt0 = insert (Text.intercalate ", " (Data.Set.toList ks))

simpleTextMessage MissingMergeType =
    
     "An empty ❰merge❱ requires a type annotation"

    

simpleTextMessage (HandlerInputTypeMismatch expr0 expr1 expr2) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr1 expr2

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

simpleTextMessage (InvalidHandlerOutputType expr0 expr1 expr2) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr1 expr2

    
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

simpleTextMessage (HandlerOutputTypeMismatch key0 expr0 key1 expr1) =
    ErrorMessages {..}
  wher
        <>  prettyDiff expr0 expr1

    
      where
        txt0 = pretty key0
        txt1 = insert expr0
        txt2 = pretty key1
        txt3 = insert expr1

simpleTextMessage (HandlerNotAFunction k expr0) = 
     "Handler is not a function"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (ConstructorsRequiresAUnionType expr0 expr1) = 
     "❰constructors❱ requires a union type"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (CantAccess lazyText0 expr0 expr1) = 
     "Not a record or a union"

    
      where
        txt0 = insert lazyText0
        txt1 = insert expr0
        txt2 = insert expr1

simpleTextMessage (CantProject lazyText0 expr0 expr1) = 
     "Not a record"

    
      where
        txt0 = insert lazyText0
        txt1 = insert expr0
        txt2 = insert expr1

simpleTextMessage (MissingField k expr0) = 
     "Missing record field"

    
      where
        txt0 = insert k
        txt1 = insert expr0

simpleTextMessage (CantAnd expr0 expr1) =
        buildBooleanOperator "&&" expr0 expr1

simpleTextMessage (CantOr expr0 expr1) =
        buildBooleanOperator "||" expr0 expr1

simpleTextMessage (CantEQ expr0 expr1) =
        buildBooleanOperator "==" expr0 expr1

simpleTextMessage (CantNE expr0 expr1) =
        buildBooleanOperator "!=" expr0 expr1

simpleTextMessage (CantInterpolate expr0 expr1) = 
     "You can only interpolate ❰Text❱"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (CantTextAppend expr0 expr1) = 
     "❰++❱ only works on ❰Text❱"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (CantListAppend expr0 expr1) = 
     "❰#❱ only works on ❰List❱s"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

simpleTextMessage (CantAdd expr0 expr1) =
        buildNaturalOperator "+" expr0 expr1

simpleTextMessage (CantMultiply expr0 expr1) =
        buildNaturalOperator "*" expr0 expr1

simpleTextMessage (NoDependentTypes expr0 expr1) = 
     "No dependent types"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

buildBooleanOperator :: Pretty a => Text -> Expr s a -> Expr s a -> ErrorMessages
buildBooleanOperator operator expr0 expr1 = 
     "❰" <> txt2 <> "❱ only works on ❰Bool❱s"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

    txt2 = pretty operator

buildNaturalOperator :: Pretty a => Text -> Expr s a -> Expr s a -> ErrorMessages
buildNaturalOperator operator expr0 expr1 = 
     "❰" <> txt2 <> "❱ only works on ❰Natural❱s"

    
      where
        txt0 = insert expr0
        txt1 = insert expr1

    txt2 = pretty operator

-- | A structured type error that includes context
data TypeError s a = TypeError
    { context     :: Context (Expr s a)
    , current     :: Expr s a
    , typeMessage :: TypeMessage s a
    }