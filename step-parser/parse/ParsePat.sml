structure ParsePat:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t list

  val pat: AstAllows.t 
           -> tokens
           -> InfixDict.t 
           -> ExpPatRestriction.t 
           -> (int, Ast.Pat.pat) parser
end =
struct 

  structure PC = ParserCombinators
  structure PS = ParseSimple
  structure PT = ParseTy
  structure Restriction = ExpPatRestriction

  type ('a, 'b) parser = ('a, 'b) PC.parser
  type tokens = Token.t list

  fun pat allows toks infdict restriction start =
    let 
      val numToks = List.length toks
      val tok i = List.nth (toks, i)
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc i =
        check (fn t => Token.Keyword rc = Token.getClass t) i 
      fun parse_oneOrMoreDelimitedByKeyword x i =
        PC.oneOrMoreDelimitedByKeyword toks x i 
      fun parse_zeroOrMoreDelimitedByKeyword x i =
        PC.oneOrMoreDelimitedByKeyword x i 
      fun parse_ty i = PT.ty toks i
end
