structure ParseSimple:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker

  type tokens = Token.t list

  val keyword: tokens -> Token.keyword -> (int, Token.t) parser
  val maybeKeyword: tokens -> Token.keyword -> (int, Token.t option) parser
  val tyvar: tokens -> (int, Token.t) parser
end = 
struct 
end
