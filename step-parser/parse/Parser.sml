structure Parser:
sig
  datatype parser_output =
    Ast of Ast.t 
  | JustComments of Token.t list

  val parse = AstAllow.t -> Token.t list -> parser_output
  val parseWithInfdict: AstAllow.t 
                        -> InfixDict.t 
                        -> Token.t list
                        -> (InfixDict.t * parser_output)
end =
struct
  structure PC = ParserCombinators
  structure PS = ParseSimple
  structure PT = ParseTy
  structure PP = ParserPat

  datatype parser_output = Ast of Ast.t | JustComments of Token.t list
  structure Restriction = ExpPatRestriction

  type ('state * 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  fun parseWithInfdict allows infdict allTokens =
    let
      val toks = List.filter (not o Token.isCommentOrWhitespace) allTokens
      val numToks = List.length toks 
      fun tok i = List.nth toks i 

      fun nyi fnme i = 
        ParserUtils.nyi toks fname i 
      
      fun check f i =
        i < numToks andalso f (tok i)

      fun is c =
        check (fn t => c = Token.getClass t)

      fun keyword rc =
        check (fn t => Token.keyword rc = Token.getClass t)


end
