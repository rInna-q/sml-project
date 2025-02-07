structure ParseTy:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t list

  val ty: tokens -> (int, Ast.Ty.ty) parser
end = 
struct 

  structure PC = ParserCombinators
  structure PS = ParseSimple
  structure PE = ParseExp

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
        
      fun parse_ty i =
        let 
          val (i, ty) =
            if check Token.isTyVar i then
              (i + 1, Ast.Ty.Var (tok i))
            else if check Token.isTyCon i then 
              parse_afterCon (tok i) (i + 1)
            else if check Token.isMaybeLongIdentifier i then
              ( i + 1
              , Ast.Ty.Con
                  {id = MaybeLongToken.make (tok i), args = Ast.SyntaxSeq.Empty}
              )
            else
              ParserUtils.tokError toks
                {pos = i, what = "Parser bug!", explain = NONE}
        in 
          parse_afterTy ty i 
        end

      fun parse_afterCon ty i =
        let 


      fun parse_afterTy ty i =
        let 
          val (again, (i, ty)) =
            if check Token.isMaybeLongTyCon i then 
              ( true
              , ( i + 1
                , Ast.Ty.Con
                    { id = MaybeLongToken.make (tok i)
                    , args = Ast.SyntaxSeq.One ty 
                    }
                )
              )
            else 
              (false, (i, ty))
        in 
          if again then parse_afterTy ty i else (i, ty)
        end
    in 
      parse_ty start
    end
end

