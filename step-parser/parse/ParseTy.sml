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
      fun parse_identifier i = PS.identifier toks i
      fun parse_oneOrMoreDelimitedByKeword x i =
        PC.oneOrMoreDelimitedByKeword toks x i 
      fun parse_exp infdict i =
        PE.exp allows toks infdict i
        
      fun parse_ty i =
        let 
          val (i, ty) =
            if check Token.isTyVar i then
              (i + 1, Ast.Ty.Var (tok i))
            else if isKeyword Token.Generic i then
              parse_afterGen (tok i) (i + 1)
            else if isKeyword Token.Aggregate i then
              parse_afterAggregate (tok i) (i + 1)
            else if isKeyword Token.Enumeration i then 
              parse_afterEnum (tok i) (i + 1)
            else if isKeyword Token.Select i then
              parse_afterSelect (tok i) (i + 1)
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

      fun parse_afterEnum enumm i =
        let 
          val (i, off) = parse_keyword Token.Of i 
          val (i, left) = parse_keyword Token.OpenParen i 

          fun parseElem i =
            let 
              val (i, elem) = parse_identifier i 
            in 
              (i, {elem = elem})
            end

          val (i, {elems, delims}) = 
            parse_oneOrMoreDelimitedByKeword
              { parseElem = parseElem
              , delim = Token.Comma
              , shouldStop = isKeyword Token.CloseParen
              } i 
          val (i, right) = parse_keyword Token.CloseParen i 
        in 
          ( i 
          , Ast.Ty.Enum 
              { enumm = enumm
              , off = off
              , left = left
              , elems = elems
              , delims = delims
              , right = right
              }
          )
        end

      fun parse_afterSelect selectt i =
        let 
          val (i, left) = parse_keyword Token.OpenParen i 

          fun parseElem i =
            let 
              val (i, elem) = parse_identifier i 
            in 
              (i, {elem = elem})
            end

          val (i, {elems, delims}) = 
            parse_oneOrMoreDelimitedByKeword
              { parseElem = parseElem
              , delim = Token.Comma
              , shouldStop = isKeyword Token.CloseParen
              } i 
          val (i, right) = parse_keyword Token.CloseParen i 
        in
          ( i 
          , Ast.Ty.Select
              { selectt = selectt
              , left = left
              , elems = elems
              , delims = delims
              , right = right
              }
          )
        end

      fun parse_afterGen gen i =
        if isKeyword Token.Colon i then 
          let 
            val (i, colon) = parse_keyword Token.Colon i 
            val (i, arg) = parse_ty i 
          in 
            ( i + 1
            , Ast.Ty.Generic
                { genericc = gen
                , elem = SOME {colon = colon, arg = arg}
                }
            )
          end
        else
          ( i + 1, Ast.Ty.Generic {genericc = genericc, elem = NONE})

      fun parse_afterAggregate agg i =
        if isKeyword Token.Colon i then
          let
            val (i, colon) = parse_keyword Token.Colon i 
            val (i, arg1) = parse_ty i 
            val (i, off) = parse_keyword Token.Of i 
            val (i, arg2) = parse_ty i 
          in 
            ( i + 1
            , Ast.Ty.Aggregate
                { agg = agg
                , elem = SOME {colon = colon, ty = arg1}
                , off = off
                , ty = arg2
                }
            )
          end
        else
          let 
            val (i, off) = parse_keyword Token.Of i 
            val (i, arg2) = parse_ty i 
          in 
            ( i + 1
            , Ast.Ty.Aggregate
                { agg = agg
                , elem = NONE
                , off = off
                , ty = arg2
                }
            )
          end
          
      fun parse_afterCon con i =
        let 
          val (i, exp) = parse_exp infdict i 
          val (i, bound) = parse_keyword Token.Of i
          val (i, optionall) = parse_keyword Token.Optional i 
          val (i, uniquee) = parse_keyword Token.Unique i 
          val (i, arg) = parse_ty i 
        in 
          ( i + 1
          , Ast.Ty.Con
              { con = con
              , bound = bound
              , off = off
              , optionall = optionall
              , uniquee = uniquee
              , arg = arg
              }
          )
        end

      fun parse_afterTy ty i =
        if isKeyword Token.OpenParen i then
          let 
            val (i, left) = parse_keyword Token.OpenParen i
            val (i, width) = parse_exp infdict i 
            val (i, right) = parse_keyword Token.CloseParen i 
            val (i, fixed) = parse_keyword Token.fixed i
          in 
            ( i + 1
            , Ast.Ty.Sized 
                { ty = ty 
                , width = SOME 
                    { left = left
                    , width = width
                    , right = right
                    , fixed = fixed
                    }
                }
            )
          end
        else
          (i, ty)

      fun parse_afterTy ty i =
        let 
          val (again, (i, ty)) =
            if isKeyword Token.OpenParen i then 
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

