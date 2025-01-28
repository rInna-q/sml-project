CM.make "lex/sources.cm";
CM.make "base/sources.cm";
val allows = 
  AstAllows.make 
    { topExp=true
    , optBar=true
    , recordPun=true
    , orPat=true
    , extendedText=true
    };
val x = Source.loadFromFile "data/242_n8324_mim_lf.exp";
val tokens = Lexer.tokens allows x;

