structure ParseTy:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t list

  val ty: tokens -> (int, Ast.Ty.ty) parser
end = 
struct 

  structure PC = ParserCombinators
  structure PS = ParseSimple

  type ('a, 'b) parser = ('a, 'b) PC.parser
  type tokens = Token.t list

  fun ty toks start = 
    let 
      val numToks = List.length toks
      fun tok i = List.nth (toks, i)
      fun check f i = 
        i < numToks andalso f (tok i)
      fun isKeyword rc i = 
        check (fn t => Token.Keyword rc = Token.getClass t) i 
      
      fun parse_keyword rc i =
        PS.keyword toks rc i 
      fun parse_zeroOrMoreDelimitedByKeword x i =
        PC.zeroOrMoreDelimitedByKeword toks x i 
        
      fun parse_tyWithRestriction

      fun parse_afterTy 
        
end

