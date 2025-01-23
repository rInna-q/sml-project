datatype interval_op = Open | Close
datatype keyword = 
    ABS | ABSTRACT | ACOS | AGGREGATE | ALIAS | AND | ANDOR | ARRAY
  | AS | ASIN | ATAN | BAG | BASED_ON | BEGIN | BINARY | BLENGTH 
  | BOOLEAN | BY | CASE | CONSTANT | CONST_E | COS | DERIVE | DIV 
  | ELSE | END | END_ALIAS | END_CASE | END_CONSTANT | END_ENTITY
  | END_FUNCTION | END_IF | END_LOCAL | END_PROCEDURE | END_REPEAT
  | END_RULE | END_SCHEMA | END_SUBTYPE_CONSTRAINT | END_TYPE 
  | ENTITY | ENUMERATION | ESCAPE | EXISTS | EXTENSIBLE | EXP 
  | FALSE | FIXED | FOR | FORMAT | FROM | FUNCTION | GENERIC
  | GENERIC_ENTITY | HIBOUND | HIINDEX | IF | IN | INSERT | INTEGER
  | INVERSE | LENGTH | LIKE | LIST | LOBOUND | LOCAL | LOG | LOG10
  | LOG2 | LOGICAL | LOINDEX | MOD | NOT | NUMBER | NVL | ODD | OF
  | ONEOF | OPTIONAL | OR | OTHERWISE | PI | PROCEDURE | QUERY 
  | REAL | REFERENCE | REMOVE | RENAMED | REPEAT | RETURN | ROLESOF
  | RULE | SCHEMA | SELECT | SELF | SET | SIN | SIZEOF | SKIP | SQRT
  | STRING | SUBTYPE | SUBTYPE_CONSTRAINT | SUPERTYPE | TAN | THEN
  | TO | TOTAL_OVER | TRUE | TYPE | TYPEOF | UNIQUE | UNKNOWN | UNTIL
  | USE | USEDIN | VALUE | VALUE_IN | VALUE_UNIQUE | VAR | WHERE 
  | WHILE | WITH | XOR
datatype exp = 
    Int of int 
  | Real of real 
  | Bool of bool option
  | String of string 
  | Qualifier of string * qualifier list
  | Interval of exp * interval_op * exp * interval_op * exp
  | Enumeration of string option * string
  | EntityConstructor of string * exp list
  | AggregateInitializer of ( exp * exp option ) list
(* QUERY EXPRESSION *)
  | Query of string * exp * exp
(* POWER OP *)
  | Pow of exp * exp 
(* UNARY OP *)
  | Plus of exp
  | Minus of exp
  | Not of exp
(* ADD LIKE OP *)
  | Add of exp * exp
  | Sub of exp * exp
  | Or of exp * exp
  | Xor of exp * exp
(* MULTIPLICATION LIKE OP *)
  | Mul of exp * exp 
  | Div of exp * exp
  | Mod of exp * exp
  | And of exp * exp
  | Con of exp * exp
(* REL LIKE OP *)
  | Greater of exp * exp
  | Less of exp * exp
  | GreaterEq of exp * exp
  | LessEq of exp * exp
  | NoEq of exp * exp
  | Eq of exp * exp
  | IntEq of exp * exp
  | IntNoEq of exp * exp
  | In of exp * exp
  | Like of exp * exp
(* QAULIFIER *)
and qualifier = 
    AttrRef of string
  | EntityRef of string
  | GroupRef of exp * exp option
(* DECLARATION *)
and declaration =
    SchemaDec of string * string option * clause list * declaration option *
    declaration list
  | ConstantDec of (string * ttype * exp) list
  | TypeDec of string * ttype * clause option 
  | EnityDec of string * exp * attribute list * attribute list * clause option *
  clause list * clause list * clause list
  | RuleDec of string * string list * declaration list * declaration option *
  declaration
  | LocalDec of (string list * ttype * exp option) list
  | FunctionDec of string * (string list * ttype) list * ttype * declaration
  list * statement list
  | ProcedureDec of string * (string list * ttype) list * declaration list *
  statement list
  | AttributeDec of string * exp list
  | RedeclaredAttrDec of attribute
  | SubtypeConstraintDec of string * string * ttype option * string list * exp
and statement = 
    AliasStmt of string * string * qualifier list * statement list
  | AssignmentStmt of string * qualifier list * exp 
  | CaseStmt of exp * (exp list * statement) list * statement option                              
  | CompoundStmt of statement list
  | EscapeStmt
  | IfStmt of exp * statement list * (statement list) option
  | NullStmt
  | ProcedureCallStmt of string 
  | RepeatStmt of (string * exp * exp * exp option) option * exp option * exp option
  | ReturnStmt of exp
  | SkipStmt
and clause =
    WhereClause of (string option * exp) list 
  | ReferenceClause of string * (string * string option) list 
  | UseClause of string * (ttype * string option) list
  | InverseClause of attribute list
  | UniqueClause of rule list
  | DeriveClause of attribute list
and attribute =
    QualifiedAttr of qualifier * qualifier
  | ExplicitAttr of declaration list * keyword option * ttype
  | DerivedAttr of declaration * ttype * exp
  | InverseAttr of declaration * (keyword * (exp * exp) option) option * string
  * string option * string
  | RedeclaredAttr of attribute * string option
  | ReferenceAttr of string * attribute
and ttype = 
(* INSTANTIABLE TYPE = 
 * ENTITY REF + TYPE REF + 
 * BINARY TYPE + BOOLEAN TYPE + 
 * INTEGER TYPE + LOGICAL TYPE +
 * NUMBER TYPE + REAL TYPE + STRING TYPE *)
    TInteger | TLogical | TNumber | TBoolean
  | TBinary of exp
  | TReal of exp
  | TString of exp
  | TAggregate of ttype
  | TArray of (exp * exp) * keyword option * keyword option * ttype
  | TBag of exp * (exp * exp) option * ttype
  | TSet of (exp * exp) option * ttype
  | TList of (exp * exp) option * keyword * ttype
  | TGeneric of string
  | TGenericArray of (exp * exp) option * keyword option * keyword option * ttype
  | TGenericBag of (exp * exp) option * ttype
  | TGenericSet of (exp * exp) option * ttype
  | TGenericList of (exp * exp) option * keyword option * ttype
  | TSelect | TEnumeration
and rule = 
    DomainRule of string option * exp
  | SupertypeRule of exp
  | UniqueRule of string option * attribute list
    
