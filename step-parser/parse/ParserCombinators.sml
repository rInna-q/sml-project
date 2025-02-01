structure ParserCombinators:
sig
  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  val zeroOrMoreDelimitedByKeyword:
    Token.t list
    -> { parseElem: (int, 'a) parser
       , delim: Token.keyword 
       , shouldStop: int peeker
       }
    -> (int, {elems: 'a list, delims: Token.t list}) parser

  val oneOrMoreDelimitedByKeyword:
    Token.t list
    -> {parseElem: (int, 'a) parser, delim: Token.keyword}
    -> {int, {elems: 'a list, delims: Token.t list}} parser

  val two: ('s, 'a) parser * ('s, 'b) parser -> ('s, ('a * 'b)) parser

  val zeroOrMoreWhile: 's peeker -> ('s, 'a) parser -> ('s, 'a list) parser

  val oneOrMoreWhile: 's peeker -> ('s, 'a) parser -> ('s, 'a list) parser

end =
struct 
  
  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  fun zeroOrMoreDelimitedByKeyword toks {parseElem, delim, shouldStop} i =
    let
      val numToks = List.length toks
      fun tok i = List.nth toks i 
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc =
        check (fn t => Token.Keyword rc = Token.getClass t)

      fun loop elems delims i =
        if shouldStop i then
          (i, elems, delims)
        else
          let 
            val (i, elem) = parseElem i 
            val elems = elem :: elems
          in 
            if isKeyword delim i then loop elems (tok i :: delims) (i + 1)
            else (i, elems, delims)
          end

      val (i, elems, delims) = loop [] [] i 
    in 
      (i, {elems = List.rev elems, delims = List.rev delims})
    end

  fun oneOrMoreDelimitedByKeyword toks {parseElem, delims} i =
    let 
      val numToks = List.length toks
      fun tok i = List.nth toks i 
      fun check f i =
        i < numToks andalso f (tok i)
      fun isKeyword rc =
        check (fn t => Token.Keyword rc = Token.getClass t)

      fun loop elems delims i =
        let 
          val (i, elems) = parseElem i 
          val elems = elem :: elems
        in 
          if isKeyword delim i then loop elems (tok i :: delims) (i + 1)
          else (i, elems, delims)
        end

      val (i, elems, delims) = loop [] [] i 
    in 
      (i, {elems = List.rev elems, delims = List.rev delims})
    end 

  fun two (p1, p2) state =
    let 
      val (state, elem1) = p1 state
      val (state, elem2) = p2 state
    in 
      (state, (elem1, elem2))
    end

  fun zeroOrMoreWhile continue parse state =
    let 
      fun loop elems state =
        if not (continue state) then
          (state, elems)
        else
          let 
            val (state, elem) = parse state 
            val elems = elem :: elems
          in 
            loop elems state 
          end
      val (state, elems) = loop [] state 
    in 
      (state, List.rev elems)
    end

  fun oneOrMoreWhile continue parse state = 
    let 
      fun loop elems state = 
        let 
          val (state, elem) = parse state 
          val elems = elem :: elems
        in 
          if not (continue state) then (state, elems) else loop elems state
        end

      val (state, elems) = loop [] state
    in 
      (state, List.rev elems)
    end
end
