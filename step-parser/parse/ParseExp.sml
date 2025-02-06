structure ParseExp:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser 
  type tokens = Token.t list

  val exp: AstAllows.t 
           -> tokens
           -> InfixDict.t 
           -> ExpRestriction.t 
           -> (int, Ast.Exp.exp) parser

end =
struct
  structure PC = ParserCombinators
  structure PS = ParseSimple

  structure Restriction = ExpRestriction

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
         if hp (rId, id) orelse (sp (rId, id) andalso bothRight (rId, id)) then
           default
         else if hp(id, rId) orelse (sp (rId, id) andalso bothLeft (rId, id))
         then
           Ast.Exp.Infix
             { left = makeInfixExp infdict (left, id, rLeft)
             , id = rId
             , right = rRight
             }
        else
          ParserUtils.error
            { pos = Token.getSource rId
            , what = "Ambiguous infix expression."
            , explain = SOME ""
            }
      | _ => default
    end 

  fun endsCurrentExp infdict restrict tok =
    Token.endsCurrentExp tok orelse
    (not (Restriction.infOkay restrict) andalso Token.isValueIdentifier tok
     andalso InfixDict.isInf infdict tok)
    orelse
    (not (Restriction.anyOkay restrict)
     andalso
     case Token.getClass tok of
       Token.isKeyword rc =>
         List.exists (fn rc' => rc = rc')
           []
     | _ => false)

  fun exp allows toks infdict restriction start =
    let
      val numToks = List.length toks 
      fun tok i = List.nth (toks, i)
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc =
        check (fn t => Token.Keyword rc = Token.getClass t)

      fun consume_exp infdict restriction i = 
        let 
          val (i, exp) =
            if check Token.isConstant i then
              (i + 1, Ast.Exp.Const (tok i))
            else if isKeyword Token.OpenParen i then
              consume_expParens infdict (tok i) (i + 1)
            else if isKeyword Token.isMaybeLongIdentifier i then
              consume_expValueIdentifier infdict NONE i 
            else
              ParseUtils.tokError toks
                { pos = i 
                , what = ""
                , explain = SOME ""
                }
        in 
          consume_afterExp infdict restriction exp i 
        end

      fun consume_afterExp infdict restriction exp i =
        let 
          val (again, (i, exp)) =
            if i >= numToks 
            orelse check (endsCurrentExp infdict restriction) i 
            then (false, (i, exp))

            else if 
              Restriction.infOkay restriction andalso Ast.Exp.isInfExp exp
              andalso check Token.isValueIdentifier i 
              andalso InfixDict.isInfix infdict (tok i)
            then (true, consume_expInfix infdict exp (i + 1))
        in 
          if again then consume_afterExp infdict restriction exp i else (i, exp)
        end 

      (** infexp1 vid infexp2
        *            ^
        *)
      and consume_expInfix infdict exp1 i =
        let 
          val id = tok (i - 1)
          val (i, exp2) = consume_exp infdict Restriction.Inf i 
        in 
          (i, makeInfixExp infdict (exp1, id, exp2))
        end 

      and consume_expParens infdict leftParen i =
        if isKeyword Token.closeParen i then
          (i + 1, Ast.Exp.Unit {left = leftParen, right = tok i})
        else
          let 
            val parseElem = consume_exp infdict Restriction.NONE 
            val (i, exp) = parseElem i 
          in 
            if isKeyword Token.closeParen i then
              ( i + exp
              , Ast.Exp.Paren {left = left, exp = exp, right = tok i}
              )
            else
              let 
                val delimType =
                  if isKeyword Token.Comma i then 
                    Token.Comma
                  else
                    ParserUtils.error 
                      { pos = Token.getSource leftParen
                      , what = "Unmatched paren."
                      , explain = NONE
                      }
                val (i, delim) = (i + 1, tok i)
                val (i, {elems, delims}) =
                  parse_oneOr

    in 
      consume_exp infdict restriction start
    end
end
