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

  type ('a, 'b) parser = ('a, 'b) PC.parser
  type tokens = Token.t list

  fun pat allows toks infdict start =
    let 
      val numToks = List.length toks
      val tok i = List.nth (toks, i)
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc i =
        check (fn t => Token.Keyword rc = Token.getClass t) i 
      fun parse_keyword rc i = 
        PS.keyword toks rc i  
      fun parse_oneOrMoreDelimitedByKeyword x i =
        PC.oneOrMoreDelimitedByKeyword toks x i 
      fun parse_zeroOrMoreDelimitedByKeyword x i =
        PC.zeroOrMoreDelimitedByKeyword x i 
      fun parse_ty i = PT.ty toks i

      fun consume_pat infdict i = 
        let 
          fun loop i =

          val (i, leftParen) = parse_keyword Token.OpenParen i 
          val (i, {elems, delims}) = 
            parse_oneOrMoreDelimitedByKeyword
              { parseElem = consume_patParensOrUnit infdict 
              , delim = Token.Comma
              } i 
          val (i, rightParen) = parse_keyword Token.CloseParen i 
        in 
          if List.length elems = 0 then 
            (i, Ast.Pat.Unit {left = leftParen, rightParen = rightParen})
          else
            (i, Ast.Pat.Paren 
                  { left = leftParen
                  , pat = {elems = elems, delims = delims}
                  , right = rightParen
                  }
            )
        end
    in 
      consume_pat infdict start
    end

end
