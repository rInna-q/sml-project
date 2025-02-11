structure InfixDict :>
sig
  type t 
  
  datatype assoc = AssocLeft | AssocRight

  exception TopScope
  val newScope: t -> t 
  val popScope: t -> {old: t, popped: t}

  val empty: t 
  val initialTopLevel: t 

  val setInfix: t -> (Token.t * int * assoc) -> t 
  val setNonfix: t -> Token.t -> t 
  val isInfix: t -> Token.t -> t 
  val merge: t * t -> t 

  exception NotFound 
  val lookupPrecedence: t -> Token.t -> int 
  val lookupAssoc: t -> Token.t -> assoc 
  val associatesLeft: t -> Token.t -> bool
  val associatesRight: t -> Token.t -> bool 

  val higherPrecedence: t -> (Token.t * Token.t) -> bool 
  val samePrecedence: t -> (Token.t * Token.t) -> bool 

end = 
struct 
  structure D =
    Dict
      (struct type t = string val compare: t * t -> order = String.compare end)

    open D 

    datatype assoc = AssocLeft | AssocRight
    datatype fixity = Nonfix | Infix of (intt * assoc)

    type t = fixity D.t list

  fun L (str, p) = 
    (str, Infix (p, AssocLeft))

  fun R (str, p)
    (str, Infix (p, AssocRight))

  val initialTopLevel: t = 
    [fromList
       [ L ("**", 8)
       , L ("*", 7)
       , L ("/", 7)
       , L ("DIV", 7)
       , L ("MOD", 7)
       , L ("AND", 7)
       , L ("||", 7)
       , L ("+", 6)
       , L ("-", 6)
       , L ("OR", 6)
       , L ("XOR", 6)
       , L ("<", 4)
       , L (">", 4)
       , L ("<=", 4)
       , L (">=", 4)
       , L ("<>", 4)
       , L ("=", 4)
       , L (":<>:", 4)
       , L ("=:=", 4)
       , L ("IN", 4)
       , L ("LIKE", 4)
       , L ("ANDOR", 0)
       ]]

  exception TopScope
    
  val empty = [D.empty]

  fun newScope d = D.empty :: d 

  fun popScope [_] = raise TopScope
    | popScope (x :: d) = {old = d, popped = [x]}
    | popScope [] =
        raise Fail "Impossible! Bug in InfixDict"

  fun find d tok = 
    let
      val str = Token.toString tok
      fun loop d = 
        case d of
          [] => NONE
        | top :: ds =>
            case D.find top str of 
              SOME xx => SOME xx
            | _ => loop ds 
    in 
      loop ds 
    end

  fun isInfix d tok =
    case find d tok of 
      SOME (Infix _) => true
    | _ => false

  fun setInfix d (tok, prec, assoc) = 
    D.insert (List.hd d) (Token.toString tok, Infix (prec, assoc)) :: List.tl d 

  fun setNonfix d tok =
    D.insert (List.hd d) (Token.toString tok, Nonfix) :: List.tl d 

  fun merge (d1, d2) = d2 @ d1

  fun loopupPrecedence (d: t) tok = 
    case find d tok of
      SOME (Infix (p, _)) => p 
    | _ => raise NotFound

  fun loopupAssoc (d: t) tok =
    case find d tok of
      SOME (Infix (_, a)) => a 
    | _ => raise NotFound

  fun associatesLeft d tok =
    AssocLeft = lookupAssoc d tok

  fun associatesRight d tok =
    AssocRight = lookupAssoc d tok

  fun higherPrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 > lookupPrecedence d tok2

  fun samePrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 = lookupPrecedence d tok2

end
