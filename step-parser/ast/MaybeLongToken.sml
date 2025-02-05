structure MaybeLongToken :>
sig
  type t 
  val make: Token.t -> t 
  val getToken: t -> Token.t 
  val isLong: t -> bool
end =
struct
  type t = Token.t 

  fun make (tok: Token.t) : t = 
    if Token.isMaybeLongIdentifier tok then
      tok 
    else
      raise Fail ""

  fun getToken tok = tok 

  fun isLong tok = Token.isMaybeLongIdentifier tok
end
