structure AstAllows:
sig
  type t 

  val make: { topExp: bool
  , optBar: bool
  , recordPun: bool
  , orPat: bool
  , extendedText: bool
  }
  -> t 
end = 
struct 
  datatype t = T of {
  topExp: bool
  , optBar: bool
  , recordPun: bool
  , orPat: bool
  , extendedText: bool
  }

  fun make x = T x
  fun topExp (T x) = #topExp x
  fun optBar (T x) = #optBar x
  fun recordPun (T x) = #recordPun x
  fun orPat (T x) = #orPat x
  fun extendedText (T x) = #extendedText x
end 

