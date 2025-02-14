structure ParseFunNameArgs:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser 
  type tokens = Token.t list

  val fname_args: AstAllow.t 
                  -> tokens 
                  -> InfixDict.t 
                  -> (int, Ast.Exp.fname_args) parser
end = 
struct 
  structure PC = ParserCombinators
  structure PS = ParseSimple 
  structure PT = ParseTy
  structure PP = ParsePat 

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser 
  type tokens = Token.t list

  fun fname_args allows toks infdict i =
    let
      val numToks = List.length toks
      fun tok i = List.nth(toks, i)
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc i =
        check (fn t => Token.Keyword rc = Token.getClass t) i 

      fun isInfixedIdentifier i =
        check Token.isIdentifier i andalso InfixDict.isInfix infdict (tok i)

      fun restOfArgs i =
        let 
          fun continue i = not (isKeyword Token.closeParen i)
        in 
          ( i 
          , Ast.Exp.InfixedFun
              { lparen = lparen
              , larg = larg 
              , id = id 

              }
          )

      fun prefixedFun opp id i = 
        let 
          val (i, args) = 
end
