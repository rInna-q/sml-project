structure ParserUtils:
sig
  val error: {pos: Source.t, what: string, explain: string option} -> 'a
  val tokError: Token.t list
                -> {pos: int, what: string, explain: string option}
                -> 'a

  val errorIfInfixNotOpped: InfixDict.t -> Token.t option -> Token.t -> unit

  val checkOptBar: AstAllows.t -> Token.t option -> string -> unit

  val nyi: Token.t list -> string -> int -> 'a 

end = 
struct 

  fun error {what, pos, explain} =
    raise Error.Error
      (Error.lineError
        {header = "PARSE ERROR", pos = pos, what = what, explain = explain})

  fun tokError toks {what, pos, explain} = 
    if pos >= List.length toks then
      let 
        val wholeSrc = Source.wholeFile (Token.getSource (List.nth (toks, 0)))
        val src = Source.drop wholeSrc (Source.length wholeSrc - 1)
      in 
        error {pos = src, what = "Unexpected end of file.", explain = NONE}
      end 
    else
      error
        { pos = Token.getSource (List.nth (toks, pos))
        , what = what
        , explain = explain
        }

  fun errorIfInfixNotOpped infdict opp vid =
    if InfixDict.isInfix infdict vid andalso not (Option.isSome opp) then
      error
        { pos = Token.getSource vid
        , what = "Infix identifier not prefaced by 'op'"
        , explain = NONE
        }
    else
      ()

  fun checkOptBar allows optbar msg = 
    case optbar of
      NONE => ()
    | SOME bar =>
        if AstAllows.optbar allows then
          ()
        else
          error
            { pos = Token.getSource bar
            , what = msg
            , explain = SOME ""
            }

  fun nyi toks fname i = 
    if i >= List.length toks then
      raise Error.Error
      (Error.lineError
      { header = "ERROR: NOT YET IMPLEMENTED"
      , pos = Token.getSource (List.nth (toks, (List.length toks - 1)))
      , what = "Unexpected EOF after token."
      , explain = SOME ""
      })
    else if i >= 0 then
      raise Error.Error
      (Error.lineError
      { header = "ERROR: NOT YET IMPLEMENTED"
      , pos = Token.getSource (List.nth (toks, i))
      , what = "Unexpected token."
      , explain = SOME ""
      })
         else
           raise Fail ""
end 
