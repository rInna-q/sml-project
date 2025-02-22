structure ParseExp:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser 
  type tokens = Token.t list

  val exp: tokens
           -> InfixDict.t 
           -> (int, Ast.Exp.exp) parser

end =
struct
  structure PC = ParserCombinators
  structure PS = ParseSimple

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t list

  fun makeInfixExp infdict (left, id, right) =
    let 
      val hp = InfixDict.higherPrecedence infdict 
      val sp = InfixDict.samePrecedence infdict
      val aLeft = InfixDict.associatesLeft infdict
      val aRight = InfixDict.associatesRight infdict

      fun bothLeft (x, y) = aLeft x andalso aLeft y
      fun bothRight (x, y) = aRight x andalso aRight y

      val default = Ast.Exp.Infix {left = left, id = id, right = right}
    in 
      case right of 
        Ast.Exp.Infix {left = rLeft, id = rId, right = rRight} =>
          if hp (rId, id) orelse (sp (rId, id) andalso bothRight (rId, id))
          then
            default
          else if hp (id, rId) orelse (sp (rId, id) andalso bothLeft (rId, id))
          then
            Ast.Exp.Infix
            { left = Ast.Exp.InfixDict infdict (left, id, rLeft)
            , id = rId
            , right = rRight
            }
          else
            ParseUtils.error 
              { pos = Token.getSource rId
              , what = "Ambiguous infix expression."
              , explain = SOME ""
              }
      | _ => default
    end 

  fun endsCurrentExp infdict tok =
    Token.endsCurrentExp tok 

  fun exp toks infdict start =
    let
      val numToks = List.length toks 
      fun tok i = List.nth (toks, i)
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc =
        check (fn t => Token.Keyword rc = Token.getClass t)

      fun parse_keyword rc i =
        PS.Keyword toks rc i 
      fun parse_maybeKeyword rc i =
        PS.maybeKeyword toks rc i 
      fun parse_vid i = PS.vid toks i 
      fun parse_longvid i = PS.longvid toks i 

      fun parse_zeroOrMoreDelimitedByKeyword x i =
        PC.zeroOrMoreDelimitedByKeyword toks x i 
      fun parse_oneOrMoreDelimitedByKeyword x i = 
        PC.oneOrMoreDelimitedByKeyword toks x i 
      
      fun consume_atExp infdict i =
        if check Token.isConstant i then 
          (i + 1, Ast.Exp.Const (tok i))
        else if check Token.isUnaryOp i then
          let 
            val opp = tok i 
            val (i, exp) = consume_atExp infdict (i + 1)
          in 
            (i + 1, Ast.Exp.Ident {opp = opp, exp = exp})
          end 
        else if isKeyword Token.OpenParen i then 
          consume_expParens infdict (tok i) (i + 1)
        else if isKeyword Token.OpenSquareBracket i then
          consume_expList infdict (tok i) (i + 1)
        else if isKeyword Token.OpenCurlyBracket i then
          consume_expInt infdict (tok i) (i + 1)
        else if isKeyword Token.Query i then 
          consume_expQuery infdict (tok i) (i + 1)
        else
          ParseUtils.tokError toks 
            { pos = i 
            , what = ""
            , explain = SOME ""
            }

      fun consume_exp infdict i = 
        let 
          val (i, exp) =
            if check Token.isConstant i then
              (i + 1, Ast.Exp.Const (tok i))
            else if check Token.isUnaryOp i then 
              consume_expIdent infdict (tok i) (i + 1) 
            else if isKeyword Token.OpenParen i then
              consume_expParens infdict (tok i) (i + 1)
            else if isKeyword Token.OpenSquareBracket i then
              consume_expList infdict (tok i) (i + 1)
            else if isKeyword Token.isMaybeLongIdentifier i then
              let 
                val (i, vid) = parse_longvid i 
              in 
                consume_afterValueIdentifier infdict vid (i + 1)
              end 
            else
              ParseUtils.tokError toks
                { pos = i 
                , what = ""
                , explain = SOME ""
                }
        in 
          consume_afterExp infdict exp i 
        end

      fun consume_expIdent infdict opp i =
        let 
          val (i, exp) = consume_exp infidict i 
        in 
          Ast.Exp.Ident {opp = opp, exp = exp}
        end 

      fun consume_afterExp infdict exp i =
        let 
          val (again, (i, exp)) =
            if i >= numToks 
            orelse check (endsCurrentExp infdict) i 
            then (false, (i, exp))

            else 
              let 
                val flag = Ast.Exp.isInfExp exp
                  andalso check Token.isIdentifier i 
                  andalso InfixDict.isInfix infdict (tok i)
              in 
                (flag, consume_expInfix infdict exp (i + 1))
              end 
        in 
          if again then consume_afterExp infdict exp i else (i, exp)
        end 

      (** infexp1 vid infexp2
        *            ^
        *)
      and consume_expInfix infdict exp1 i =
        let 
          val id = tok (i - 1)
          val (i, exp2) = consume_exp infdict i 
        in 
          (i, makeInfixExp infdict (exp1, id, exp2))
        end 

      and consume_expParens infdict leftParen i =
        if isKeyword Token.closeParen i then
          (i + 1, Ast.Exp.Unit {left = leftParen, right = tok i})
        else
          let 
            val parseElem = consume_exp infdict 
            val (i, exp) = parseElem i 
            val (i, rightParen) = parse_keyword Token.rightParen i 
          in 
            Ast.Exp.Paren
              { left = leftParen
              , exp = exp 
              , right = rightParen
              }
          end 

      and consume_expList infdict leftSquareBracket =


      and consume_afterValueIdentifier infdict vid i =
        if check Token.OpenParen i then
          let 
            val (i, left) = parse_keyword Token.OpenParen i 
            val (i, pat) = parse_pat i 
            val (i, right) = parse_keyword Token.closeParen i 
          in 
          end 
        else
          Ast.Exp.
    in 
      consume_exp infdict start
    end
end
