structure Token :>
sig 
  datatype keyword = 
  (* KEYWORD *)
    Abs
  | Abstract
  | Acos
  | Aggregate
  | Alias 
  | And 
  | Andor 
  | Array 
  | As 
  | Asin 
  | Atan 
  | Bag 
  | Based_in
  | Begin 
  | Binary 
  | Blength
  | Boolean 
  | By 
  | Case 
  | Constant 
  | Const_e
  | Cos 
  | Derive
  | Div 
  | Else 
  | End 
  | End_alias
  | End_case 
  | End_constant 
  | End_entity
  | End_function 
  | End_if
  | End_local
  | End_procedure
  | End_repeat 
  | End_rule
  | End_schema 
  | End_subtype_constraint
  | End_type 
  | Entity 
  | Enumeration
  | Escape 
  | Exists
  | Extensible
  | Exp 
  | False
  | Fixed 
  | For 
  | Format
  | From 
  | Function  
  | Generic 
  | Generic_entity
  | Hibound 
  | Hiindex
  | If 
  | In 
  | Insert 
  | Integer 
  | Inverse 
  | Length
  | Like 
  | List 
  | Lobound
  | Local  
  | Log 
  | Log10
  | Log2 
  | Logical
  | Loindex
  | Mod
  | Not
  | Number 
  | Nvl
  | Odd 
  | Of 
  | Oneof 
  | Optional
  | Or
  | Otherwise
  | Pi
  | Procedure   
  | Query
  | Real 
  | Reference
  | Remove 
  | Renamed
  | Repeat 
  | Return
  | Rolesof 
  | Rule 
  | Schema  
  | Select
  | Self 
  | Set 
  | Sin 
  | Sizeof
  | Skip 
  | Sqrt
  | String 
  | Subtype  
  | Subtype_constraint
  | Supertype
  | Tan 
  | Then 
  | To 
  | Total_over
  | True
  | Type  
  | Typeof
  | Unique 
  | Unknown
  | Until 
  | Use 
  | Usedin
  | Value 
  | Value_in
  | Value_unique 
  | Var
  | Where 
  | While
  | With 
  | Xor
  (* SYMBOL *)
  | OpenParen 
  | CloseParen
  | OpenSquareBracket 
  | CloseSquareBracket
  | OpenCurlyBracket 
  | CloseCurlyBracket
  | Equal 
  | Comma
  | Semicolon 
  | Colon
  | Backslash
  
  datatype class =
    Comment
  | Keyword of keyword
  | IntegerConstant
  | RealConstant
  | CharConstant
  | StringConstant
  | Identifier
  | LongIdentifier
  | Whitespace

  type token 
  type t = token 

  val getClass: token -> class
  val getSource: token -> Source.t 

  val toString: token -> string 

  val isKeyword: token -> bool 
  val isStringConstant: token -> bool
  val isComment: token -> bool
  val isWhitespace: token -> bool
  val isCommentOrWhitespace: token -> bool 
  val isStar: token -> bool
  val isOpenParen: token -> bool
  val isSemicolon: token -> bool
  val isIdentifier: token -> bool

  structure Pretoken:
  sig
    type t 
    type pretoken = t 
    
    val getSource: pretoken -> Source.t 
    val getClass: pretoken -> class

    val make: Source.t -> class -> pretoken
    val keyword: Source.t -> keyword -> pretoken
    val identifier: Source.t -> pretoken
    val keywordOrIdentifier: Source.t -> pretoken
  end 

  val fromPre: Pretoken.t -> token
  val makeGroup: Pretoken.t list -> token list
  
end =
struct

  datatype keyword = 
  (* KEYWORD *)
    Abs
  | Abstract
  | Acos
  | Aggregate
  | Alias 
  | And 
  | Andor 
  | Array 
  | As 
  | Asin 
  | Atan 
  | Bag 
  | Based_in
  | Begin 
  | Binary 
  | Blength
  | Boolean 
  | By 
  | Case 
  | Constant 
  | Const_e
  | Cos 
  | Derive
  | Div 
  | Else 
  | End 
  | End_alias
  | End_case 
  | End_constant 
  | End_entity
  | End_function 
  | End_if
  | End_local
  | End_procedure
  | End_repeat 
  | End_rule
  | End_schema 
  | End_subtype_constraint
  | End_type 
  | Entity 
  | Enumeration
  | Escape 
  | Exists
  | Extensible
  | Exp 
  | False
  | Fixed 
  | For 
  | Format
  | From 
  | Function  
  | Generic 
  | Generic_entity
  | Hibound 
  | Hiindex
  | If 
  | In 
  | Insert 
  | Integer 
  | Inverse 
  | Length
  | Like 
  | List 
  | Lobound
  | Local  
  | Log 
  | Log10
  | Log2 
  | Logical
  | Loindex
  | Mod
  | Not
  | Number 
  | Nvl
  | Odd 
  | Of 
  | Oneof 
  | Optional
  | Or
  | Otherwise
  | Pi
  | Procedure   
  | Query
  | Real 
  | Reference
  | Remove 
  | Renamed
  | Repeat 
  | Return
  | Rolesof 
  | Rule 
  | Schema  
  | Select
  | Self 
  | Set 
  | Sin 
  | Sizeof
  | Skip 
  | Sqrt
  | String 
  | Subtype  
  | Subtype_constraint
  | Supertype
  | Tan 
  | Then 
  | To 
  | Total_over
  | True
  | Type  
  | Typeof
  | Unique 
  | Unknown
  | Until 
  | Use 
  | Usedin
  | Value 
  | Value_in
  | Value_unique 
  | Var
  | Where 
  | While
  | With 
  | Xor
  (* SYMBOL *)
  | OpenParen 
  | CloseParen
  | OpenSquareBracket 
  | CloseSquareBracket
  | OpenCurlyBracket 
  | CloseCurlyBracket
  | Equal 
  | Comma
  | Semicolon 
  | Colon
  | Backslash
  
  datatype class =
    Comment
  | Keyword of keyword
  | IntegerConstant
  | RealConstant
  | CharConstant
  | StringConstant
  | Identifier
  | LongIdentifier
  | Whitespace

  type pretoken = class WithSource.t 

  type token = {idx: int, context: pretoken list}
  type t = token

  fun make src class = WithSource.make {value = class, source = src}

  fun keyword src kclass = WithSource.make 
    {value = Keyword kclass, source = src} 

  fun identifier src = WithSource.make {value = Identifier, source = src}

  fun getClass ({idx, context}: token) =
    WithSource.valOf (List.nth (context, idx))

  fun getSource ({idx, context}: token) =
    WithSource.srcOf (List.nth (context, idx))

  fun toString tok =
    let val src = getSource tok
    in CharVector.tabulate (Source.length src, Source.nth src)
    end

  fun tryKeyword src = 
    let 
      val str = CharVector.tabulate (Source.length src, Source.nth src)
      fun k kclass = SOME kclass
    in 
      case str of 
        "ABS" => k Abs
      | "ABSTRACT" => k Abstract
      | "ACOS" => k Acos
      | "AGGREGATE" => k Aggregate
      | "ALIAS" => k Alias
      | "AND" => k And
      | "ANDOR" => k Andor
      | "ARRAY" => k Array
      | "AS" => k As 
      | "ASIN" => k Asin
      | "ATAN" => k Atan
      | "BAG" => k Bag
      | "BASED_IN" => k Based_in
      | "BEGIN" => k Begin
      | "BINARY" => k Binary
      | "BLENGTH" => k Blength
      | "BOOLEAN" => k Blength
      | "BY" => k By 
      | "CASE" => k Case
      | "CONSTANT" => k Constant
      | "CONST_E" => k Const_e
      | "COS" => k Cos
      | "DERIVE" => k Derive
      | "DIV" => k Div
      | "ELSE" => k Else
      | "END" => k End
      | "END_ALIAS" => k End_alias
      | "END_CASE" => k End_case
      | "END_CONSTANT" => k End_constant
      | "END_ENTITY" => k End_entity
      | "END_FUNCTION" => k End_function
      | "END_IF" => k End_if
      | "END_LOCAL" => k End_local
      | "END_PROCEDURE" => k End_procedure
      | "END_REPEAT" => k End_repeat
      | "END_RULE" => k End_rule
      | "END_SCHEMA" => k End_schema
      | "END_SUBTYPE_CONSTRAINT" => k End_subtype_constraint
      | "END_TYPE" => k End_type
      | "ENTITY" => k Entity
      | "ENUMERATION" => k Enumeration
      | "ESCAPE" => k Escape
      | "EXISTS" => k Exists
      | "EXTENSIBLE" => k Extensible
      | "EXP" => k Exp
      | "FALSE" => k False
      | "FIXED" => k Fixed
      | "FOR" => k For
      | "FORMAT" => k Format
      | "FROM" => k From
      | "FUNCTION" => k Function
      | "GENERIC" => k Generic
      | "GENERIC_ENTITY" => k Generic_entity
      | "HIBOUND" => k Hibound
      | "HIINDEX" => k Hiindex
      | "IF" => k If 
      | "IN" => k In 
      | "INSERT" => k Insert
      | "INTEGER" => k Integer
      | "INVERSE" => k Inverse
      | "LENGTH" => k Length
      | "LIKE" => k Like
      | "LIST" => k List
      | "LOBOUND" => k Lobound
      | "LOCAL" => k Local
      | "LOG" => k Log 
      | "LOG10" => k Log10
      | "LOG2" => k Log2
      | "LOGICAL" => k Logical
      | "LOINDEX" => k Loindex
      | "MOD" => k Mod
      | "NOT" => k Not
      | "NUMBER" => k Number
      | "NVL" => k Nvl
      | "ODD" => k Odd
      | "OF" => k Of 
      | "ONEOF" => k Oneof
      | "OPTIONAL" => k Optional
      | "OR" => k Or 
      | "OTHERWISE" => k Otherwise
      | "PI" => k Pi
      | "PROCEDURE" => k Procedure
      | "QUERY" => k Query
      | "REAL" => k Real
      | "REFERENCE" => k Reference
      | "REMOVE" => k Remove
      | "RENAMED" => k Renamed
      | "REPEAT" => k Repeat
      | "RETUREN" => k Return
      | "ROLESOF" => k Rolesof
      | "RULE" => k Rule
      | "SCHEMA" => k Schema
      | "SELECT" => k Select
      | "SELF" => k Self
      | "SET" => k Set
      | "SIN" => k Sin
      | "SIZEOF" => k Sizeof
      | "SKIP" => k Skip
      | "SQRT" => k Sqrt
      | "STRING" => k String
      | "SUBTYPE" => k Subtype
      | "SUBTYPE_CONSTRAINT" => k Subtype_constraint
      | "SUPERTYPE" => k Supertype
      | "TAN" => k Tan
      | "THEN" => k Then
      | "TO" => k To 
      | "TOTAL_OVER" => k Total_over
      | "TRUE" => k True
      | "TYPE" => k Type
      | "TYPEOF" => k Typeof
      | "UNIQUE" => k Unique
      | "UNKNOWN" => k Unknown
      | "UNTIL" => k Until
      | "USE" => k Use
      | "USEDIN" => k Usedin
      | "VALUE" => k Value
      | "VALUE_IN" => k Value_in
      | "VALUE_UNIQUE" => k Value_unique
      | "VAR" => k Var
      | "WHERE" => k Where
      | "WHILE" => k While
      | "WITH" => k With
      | "XOR" => k Xor
      | "=" => k Equal
      | ":" => k Colon
      | _ => NONE
    end
      
  fun keywordOrIdentifier src = 
    case tryKeyword src of 
      SOME k => keyword src k 
    | NONE => identifier src 

  fun isKeyword (tok: token) = 
    case getClass tok of 
      Keyword _ => true 
    | _ => false 

  fun isStringConstant tok =
    case getClass tok of 
      StringConstant => true 
    | _ => false

  fun isComment tok =
    case getClass tok of 
      Comment => true 
    | _ => false

  fun isWhitespace tok =
    case getClass tok of 
      Whitespace => true
    | _ => false

  fun isCommentOrWhitespace tok = isComment tok orelse isWhitespace tok 

  fun isStar tok =
    let val src = getSource tok 
    in Source.length src = 1 andalso Source.nth src 0 = #"*"
    end
    
  fun isOpenParen tok =
    case getClass tok of 
      Keyword OpenParen => true 
    | _ => false

  fun isSemicolon tok =
    case getClass tok of 
      Keyword Semicolon => true 
    | _ => false

  fun isIdentifier tok =
    case getClass tok of 
      Identifier => true
    | _ => false

  fun makeGroup (s: pretoken list): token list =
    List.tabulate (List.length s, fn i => {idx = i, context = s})

  fun fromPre (t: pretoken) = 
    List.nth ((makeGroup [t]), 0)
    
  structure Pretoken =
  struct
    type t = pretoken
    type pretoken = pretoken

    fun getSource p = WithSource.srcOf p 
    fun getClass p = WithSource.valOf p 

    val make = make
    val keyword = keyword
    val identifier = identifier
    val keywordOrIdentifier = keywordOrIdentifier

  end 

end 
