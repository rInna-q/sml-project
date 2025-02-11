structure ParseSimple:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker

  type tokens = Token.t list

  val keyword: tokens -> Token.keyword -> (int, Token.t) parser
  val maybeKeyword: tokens -> Token.keyword -> (int, Token.t option) parser
  val tyvar: tokens -> (int, Token.t) parser
  val tyvars: tokens -> (int, Token.t Ast.SyntaxSeq.t) parser
 
  val genericLabel: tokens -> (int, Token.t) parser
  val bagLabel: tokens -> (int, Token.t) parser
  val setLabel: tokens -> (int, Token.t) parser
  val listLabel: tokens -> (int, Token.t) parser 
  val arrayLabel: tokens -> (int, Token.t) parser
  val aggregateLabel: tokens -> (int, Token.t) parser
  val enumerationLabel: tokens -> (int, Token.t) parser
  val selectLabel: tokens -> (int, Token.t) parser


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
    if isKeyword toks rc i then (i + 1, SOME (List.nth (toks, i))) else (i, NONE)

  fun genericLabel toks i =
    if check toks Token.isGenericLabel i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun bagLabel toks i =
    if check toks Token.isBagLabel i then 
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun setLabel toks i =
    if check toks Token.isSetLabel i then 
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun listLabel toks i =
    if check toks Token.isListLabel i then 
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun arrayLabel toks i =
    if check toks Token.isArrayLabel i then 
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks 
        {pos = i, what = "", explain = NONE}

  fun aggregateLabel toks i =
    if check toks Token.isAggregateLabel i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun enumerationLabel toks i =
    if check toks Token.isEnumerationLabel i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks
        {pos = i, what = "", explain = NONE}

  fun selectLabel toks i =
    if check toks Token.isSelectLabel i then
      (i + 1, List.nth (toks, i))
    else
      ParseUtils.tokError toks 
        {pos = i, what = "", explain = NONE}

end
