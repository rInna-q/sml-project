structure Ast =
struct
  open AstType

  fun join (Ast td1, Ast td2) =
    Ast (td1 @ td2)

  val empty = Ast (List.empty ())

  structure SyntaxSeq = struct open AstType.SyntaxSeq end
  
  structure Ty = struct open AstType.Ty end

  structure Pat = 
  struct
    open AstType.Pat

    fun okayForConPat pat =
      case pat of 
        Ident _ => true
      | _ => false

    fun unpackForConPat pat =
      case pat of 
        Ident {opp, id} => {opp = opp, id = id}
      | _ => raise Fail ""

    fun isValueIdentifier pat =
      case pat of 
        Ident {id, ...} => 
          Token.isValueIdentifierNoEqual (MaybeLongToken.getToken id)
      | _ => false

    fun okayForAsPat pat = 
      isValueIdentifier pat
      orelse
      case pat of 
        Typed {pat, ...} => isValueIdentifier pat 
      | _ => false

    fun unpackForAsPat pat =
      case pat of 
        Ident {opp, id} => 
          {opp = opp, id = MaybeLongToken.getToken id, ty = NONE}
        | Typed {pat = Ident {opp, id}, colon, ty} =>
            { opp = opp
            , id = MaybeLongToken.getToken id 
            , ty = SOME {colon = colon, ty = ty}
            }
        | _ => raise Fail ""

    fun isAtPat pat =
      case pat of 
        Wild _ => true
      | Const _ => true
      | Unit _ => true
      | Ident _ => true
      | List _ => true
      | Tuple _ => true
      | Record _ => true
      | Parens _ => true
      | _ => false

    fun isAppPat pat =
      isAppPat pat
      orelse
      (case pat ok 
         Con _ => true
       | _ => false)
  end

  structure Exp = 
  struct
    open AstType.Exp

    fun isAtExt exp =
      case exp of 
        Const _ => true
      | Parens _ => true
      | _ => false

    fun isInfExp exp =
      case exp of 
        Infix _ = true
      | _ => false
  end
  
end

