structure ParseSimple:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker

  type tokens = Token.t list

  val keyword: tokens -> Token.keyword -> (int, Token.t) parser
  val maybeKeyword: tokens 
                    -> Token.keyword 
                    -> (int, Token.t option) parser
   
  val tyvar: tokens -> (int, Token.t) parser
  val tyvars: tokens -> (int, Token.t Ast.SyntaxSeq.t) parser
  val identifier: tokens -> (int, Token.t) parser

end = 
struct 

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker
  type tokens = Token.t list

  fun check toks f i = 
    let 
      val numToks = List.length toks
      fun tok i = List.nth (toks, i)
    in 
      i < numToks andalso f (tok i)
    end

  fun isKeyword toks rc i = 
    check toks (fn t => Token.Keyword rc = Token.getClass t) i 

  fun keyword toks rc i = 
    if isKeyword toks rc i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks 
        { pos = i 
        , what = ""
        , explain = NONE
        }

  fun maybeKeyword toks rc i =
    if isKeyword toks rc i then
      let 
        val tok = List.nth (toks, i)
      in 
        (i + 1, SOME tok) 
      end
    else 
      (i, NONE)

  fun tyvar toks i = 
    if check toks Token.isTyVar i then 
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks 
        { pos = i 
        , what = ""
        , explain = SOME ""
        }

  fun tyvars toks i =
    if 
      check toks Token.isTyVar i 
    then
      (i + 1, Ast.SyntaxSeq.One (List.nth (toks, i)))
    else if 
      not (isKeyword toks Token.OpenParen i 
           andalso check toks Token.isTyVar (i + 1))
    then
      (i, Ast.SyntaxSeq.Empty)
    else
      let 
        val (i, openParen) = (i + 1, List.nth (toks, i))
        val (i, {elems, delims}) =
          ParserCombinators.oneOrMoreDelimitedByKeyword toks
            {parseElem = tyvar toks, delim = Token.Comma} i 
        val (i, closeParen) = keyword toks Token.closeParen i 
      in 
        ( i 
        , Ast.SyntaxSeq.Many
            { left = openParen
            , right = closeParen
            , elems = elems
            , delims = delims
            }
        )
      end

  fun identifier toks i =
    if check toks Token.isIdentifier i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks 
        {pos = i, what = "", explain = NONE}

end
