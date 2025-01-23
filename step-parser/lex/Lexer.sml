structure Lexer: 
sig
  val next: AstAllows.t -> Source.t -> Token.Pretoken.t option
  val tokens: AstAllows.t -> Source.t -> Token.t list
end =
struct

  val backslash = #"\\"

  fun success tok = SOME tok 

  fun error {pos, what, explain} =
    raise Error.Error 
      (Error.lineError 
        {header = "SYNTAX ERROR", pos = pos, what = what, explain = explain})

  fun next allows src: Token.Pretoken.t option = 
    let 
      val startOffset = Source.absoluteStartOffset src
      val src = Source.wholeFile src
      fun isEndOfFileAt s = s > Source.length src 
      fun get i = Source.nth src i 
      fun slice (i, j) = Source.slice src (i, j - i)
      fun mk x (i, j) = Token.Pretoken.make (slice (i, j)) x
      fun mkr x (i, j) = Token.Pretoken.keyword (slice (i, j)) x
      fun check f i =
        i < Source.length src andalso f (get i)
      fun is c = check (fn c' => c = c')
      
      datatype newcursor = EndOfChar of int | EndOfFormatEscape of int


      fun advance_oneCharOrEscapeSequenceInString s (args as {stringStart}) =
        (* meet escape sequence *)
        if is backslash s then
          advance_inStringEscapeSequence (s + 1) args
         
        (* end of the string *)
        else if is #"\"" s then NONE
             
        (* meet undesired end *)
        else if is #"\n" s orelse isEndOfFileAt s then
          error 
            { pos = slice (stringStart, stringStart + 1)
            , what = "Unclosed string."
            , explain = NONE
            }

        (* true character *)
        else SOME (EndOfChar (s + 1))

      and advance_inStringEscapeSequence s (args as {stringStart}) =
        if check LexUtils.isValidSingleEscapeChar s then 
          SOME (EndOfChar (s + 1))
        else 
          error
            { pos = slice (s - 2, s - 1)
            , what = "Invalid escape sequence."
            , explain = SOME ""
            }
          
        
      and advance_toEndOfString s (args as {stringStart}) =
        case advance_oneCharOrEscapeSequenceInString s args of 
             SOME (EndOfChar s') => advance_toEndOfString s' args
           | SOME (EndOfFormatEscape s') => advance_toEndOfString s' args
           | NONE => 
               if is #"\"" s then s + 1
               else
                 raise Error.Error 
                   { header = "BUG!"
                   , content = []
                   }

      and advance_oneCharInString s args =
        case advance_oneCharOrEscapeSequenceInString s args of 
          SOME (EndOfChar s') => SOME s'
        | SOME (EndOfFormatEscape s') => advance_oneCharInString s' args 
        | NONE => NONE

      fun loop_topLevel s = 
        if isEndOfFileAt s then
          NONE
        else
          case get s of 
            #"(" => loop_afterOpenParen (s + 1)
          | #")" => success (mkr Token.CloseParen (s, s + 1))
          | #"[" => success (mkr Token.OpenSquareBracket (s, s + 1))
          | #"]" => success (mkr Token.CloseSquareBracket (s, s + 1))
          | #"{" => success (mkr Token.OpenCurlyBracket (s, s + 1))
          | #"}" => success (mkr Token.CloseCurlyBracket (s, s + 1))
          | #"," => success (mkr Token.Comma (s, s + 1))
          | #";" => success (mkr Token.Semicolon (s, s + 1))
          | #"\"" =>
              let val s' = advance_toEndOfString (s + 1) {stringStart = s}
              in success (mk Token.StringConstant (s, s'))
              end 
          | #"0" => loop_afterZero (s + 1)  
          | c =>
              if LexUtils.isDecDigit c then
                loop_decIntegerConstant (s + 1) {constStart = s}
              else if LexUtils.isLetter c then
                loop_alphanumId (s + 1)
                  {idStart = s, startsPrime = false, longStart = NONE}
              else if Char.isSpace c then
                loop_whitespace {start = s} (s + 1)
              else error 
                { pos = slice (s, s + 1)
                  , what = "Unexpected character."
                  , explain = SOME "Perhaps from unsupported character-set?"
                } 

      and loop_whitespace {start} i =
        if check Char.isSpace i then loop_whitespace {start = start} (i + 1)
        else success (mk Token.Whitespace (start, i))

      and loop_charConstant s =
        case advance_oneCharInString s {stringStart = s - 1} of 
          SOME s' => 
            if is #"\"" s' then
              success (mk Token.CharConstant (s - 2, s' + 1))
            else
              error {
                pos = slice (s', s' + 1)
                , what = "Character constant contains more than one character."
                , explain = NONE 
                }
        | NONE => 
            error {
              pos = slice (s - 2, s + 1)
              , what = "Character constant is empty."
              , explain = NONE
              }

      and loop_symbolicId s (args as {idStart, longStart}) =
        if check LexUtils.isSymbolic s then
          loop_symbolicId (s + 1) args
        else
          let 
            val srcHere = slice (idStart, s)
            val tok = Token.Pretoken.keywordOrIdentifier srcHere
            val isQualified = Option.isSome longStart
          in 
            if Token.isKeyword (Token.fromPre tok) andalso isQualified then
              error{
                pos = srcHere
                , what = ""
                , explain = SOME ""
                }
            else if not isQualified then
              success tok
            else
              NONE
          end 

      and loop_alphanumId s (args as {idStart, startsPrime, longStart}) =
        if check LexUtils.isAlphaNumPrimeOrUnderscore s then
          loop_alphanumId (s + 1) args
        else
          let 
            val srcHere = slice (idStart, s)
            val tok = Token.Pretoken.keywordOrIdentifier srcHere
            val isQualified = Option.isSome longStart
          in 
            if Token.isKeyword (Token.fromPre tok)
            andalso (isQualified orelse is #"." s) then
              error {
                pos = srcHere
                , what = "Unexpected keyword."
                , explain = SOME ""
                }
            else if is #"." s andalso startsPrime then
              loop_continueLongIdentifier (s + 1)
                {longStart = if isQualified then longStart else SOME idStart}
            else if not isQualified then
              success tok
            else
              NONE
          end 

      and loop_continueLongIdentifier s {longStart} =
        if check LexUtils.isSymbolic s then
          loop_symbolicId (s + 1) {idStart = s, longStart = longStart}
        else if check LexUtils.isLetter s then
          loop_alphanumId (s + 1)
            {idStart = s, startsPrime = false, longStart = longStart}
        else
          error
            { pos = slice (s, s + 1)
            , what = "Unexpected character."
            , explain = SOME ""
            }

      and loop_afterZero s =
        if is #"x" s andalso check LexUtils.isHexDigit (s + 1) then
          loop_hexIntegerConstant (s + 2) {constStart = s - 1}
        else if is #"." s then
          loop_realConstantAfterDot (s + 1) {constStart = s - 1}
        else if is #"e" s orelse is #"E" s then
          loop_realConstantAfterExponent (s + 1) {constStart = s - 1}
        else if check LexUtils.isDecDigit s then
          loop_decIntegerConstant (s + 1) {constStart = s - 1}
        else
          success (mk Token.IntegerConstant (s - 1, s))


      and loop_decIntegerConstant s (args as {constStart}) =
        if check LexUtils.isDecDigit s then
          loop_decIntegerConstant (s + 1) args
        else if is #"." s then
          loop_realConstantAfterDot (s + 1) args
        else if is #"e" s orelse is #"E" s then
          loop_realConstantAfterExponent (s + 1) args
        else
          success (mk Token.IntegerConstant (constStart, s))
      
      and loop_realConstantAfterDot s (args as {constStart}) = 
        if check LexUtils.isDecDigit s then
          loop_realConstant (s + 1) args
        else
          error 
            { pos = slice (constStart, s)
            , what = "Invalid real constant."
            , explain = SOME ""
            }

      and loop_realConstant s (args as {constStart}) = 
        if check LexUtils.isDecDigit s then
          loop_realConstant (s + 1) args
        else if is #"e" s orelse is #"E" s then
          loop_realConstantAfterExponent (s + 1) args
        else
          success (mk Token.RealConstant (constStart, s))
          
      and loop_realConstantAfterExponent s {constStart} =
        let 
          fun walkThroughDigits i =
            if check LexUtils.isDecDigit i then walkThroughDigits (i + 1)
            else i 
          val s' = walkThroughDigits (if is #"-" s then s + 1 else s)
        in 
          success (mk Token.RealConstant (constStart, s'))
        end 

      and loop_hexIntegerConstant s (args as {constStart}) =
        if check LexUtils.isHexDigit s then
          loop_hexIntegerConstant (s + 1) args
        else
          success (mk Token.IntegerConstant (constStart, s))

      and loop_afterOpenParen s = 
        if is #"*" s then
          loop_inComment (s + 1) {commentStart = s - 1, nesting = 1}
        else
          success (mkr Token.OpenParen (s - 1, s))

      and loop_inComment s {commentStart, nesting} =
        if nesting = 0 then
          success (mk Token.Comment (commentStart, s))
        else if is #"(" s andalso is #"*" (s + 1) then
          loop_inComment (s + 2)
            {commentStart = commentStart, nesting = nesting + 1}
        else if is #"*" s andalso is #")" (s + 1) then
          loop_inComment (s + 2)
            {commentStart = commentStart, nesting = nesting - 1}
        else if isEndOfFileAt s then
          error 
            { pos = slice (commentStart, commentStart + 2)
            , what = "Unclosed comment."
            , explain = NONE
            }
        else
          loop_inComment (s + 1)
            {commentStart = commentStart, nesting = nesting}

    in 
      loop_topLevel startOffset
    end 

  fun tokens allows src = 
    let 
      val startOffset = Source.absoluteStartOffset src
      val endOffset = Source.absoluteEndOffset src
      val src = Source.wholeFile src

      fun tokEndOffset tok =
        Source.absoluteEndOffset (Token.Pretoken.getSource tok)

      fun finish acc =
        Token.makeGroup (List.rev acc)

      fun loop acc offset =
        if offset >= endOffset then finish acc
        else
          case next allows (Source.drop src offset) of 
            NONE => finish acc
          | SOME tok => 
              loop (tok :: acc) (tokEndOffset tok)
    in 
      loop [] startOffset
    end 
end
