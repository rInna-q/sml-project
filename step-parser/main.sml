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
val x = Source.loadFromFile "data/entity.stp";
val tokens = Lexer.tokens allows x;

