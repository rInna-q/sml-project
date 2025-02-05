structure AstType = 
struct

  structure SyntaxSeq =
  struct
    datatype 'a t =
      Empty
    | One of 'a 
    | Many of
        { left: Token.t 
        , elems: 'a list
        , delims: Token.t list
        , right: Token.t 
        }
  end

  structure Pat =
  struct
    datatype typelist =
      List of 
        { varr: Token.t option
        , elems: Token.t list
        , delims: Token list
        , colon: Token .t 
        , ty: Ty.t 
        }

    and pat = 
      Unit of {left: Token.t, right: Token.t}
    | Tuple of 
        { left: Token.t
        , elems: typelist list
        , delims: Token list 
        , right: Token.t
        }
    | Con of {id: MaybeLongToken.t, atpat: pat}

    type t = pat
  end 

  structure Exp =
  struct
    datatype exp =
      Const of Token.t 
    | Parens of 
         { left: Token.t 
         , exp: exp
         , right: Token.t 
         }
    | Infix of 
         { left: exp
         , id: Token.t 
         , right: exp
         }
    type t = exp
  end

  type bound = 
    { left: Token.t 
    , bound_1: Exp.t  
    , colon: Token.t 
    , bound_2: Exp.t  
    , right: Token.t 
    }

  type width = 
    { left: Token.t 
    , width: Exp.t 
    , right: Token.t 
    , fixed: Token.t 
    }

  type interval = 
    { left: Token.t 
    , low: Expxp.t 
    , op1: Token.t 
    , item: Exp.t  
    , op2: Token.t 
    , high: Exp.t 
    }

  structure Ty = 
  struct
    datatype ty =
    (* BOOLEAN | INTEGER | LOGICAL | NUMBER | REAL *)
      Var of Token.t 
    (* GENERIC *)
    | Generic of {genericc: Token.t, elem: {colon: Token.t, arg: ty} option}
    (* STRING | BINARY *)
    | Sized of {ty: ty, width: width option}
    (* BAG | SET | LIST | ARRAY | GENERAL *)
    | Con of 
         { con: Token.t
         , bound: bound option
         , off: Token.t
         , optionall: Token.t 
         , uniquee: Token.t 
         , arg: ty}
    (* AGGREGATE *)
    | Agg of {agg: Token.t, elem: {colon: Token.t, ty: ty} option, off: Token.t, ty: ty}
    (* ENUMERATION *)
    | Enum of 
         { enumm: Token.t
         , off: Token.t
         , left: Token.t
         , elems: Token.t list
         , delims: Token.t list 
         , right: Token.t
         }
    (* SELECT *)
    | Select of 
         { selectt: Token.t 
         , left: Token.t 
         , elems: Token.t list 
         , delims: Token.t list 
         , right: Token.t
         }

    type t = ty 
  end

  (** ====================================
    * Statements, expressions and declarations
    *)
  structure Stmt = 
  struct
    type fname_args = 
      {id: Token.t, farg: Pat.t}
    
    type pname_args =
      {id: Token.t, parg: Pat.t}

    type super =
      { off: Token.t
      , left: Token.t
      , se: Exp.t 
      , right: Token.t 
      }

    type subsuper = {super: super, sd: dec option}

    type abssuper = {abstract: Token.t, supertype: Token.t}


    type tover = 
      { to: Token.t 
      , left: Token.t 
      , elems: Token.t list
      , delims: Token.t list
      , right: Token.t}

    type ('alg, 'stmt) fvalbind =
      { fname_args: fname_args
      , ty: {colon: Token.t, ty: Ty.t} 
      , alg: 'alg
      , stmt: 'stmt
      }

    type ('alg, 'stmt) pvalbind = 
      { pname_args: pname_args
      , alg: 'alg
      , stmt: 'stmt
      }

    type tvalbind =
      { tname: Token.t  
      , eq: Token.t 
      , ut: Ty.t 
      , wc: option
      }

    type evalbind =
      { ename: Token.t 
      , ss: subsuper
      , ea: attr list
      , dc: clause option
      , ic: clause option
      , uc: clause option
      , wc: clause option
      } 

    type svalbind =
      { sname: Token.t
      , forr: Token.t 
      , er: Token.t
      , abs: abssuper
      , to: tover
      , se: Exp.t
      }

    type cvalbind = 
      { cname: Token.t 
      , colon: Token.t 
      , ty: Ty.t 
      , colonequal: Token.t 
      , elems: Exp.t list
      } list

    datatype attr =
      ExpliAttr of 
          { elems: dec list
          , delims: Token.t 
          , colon: Token.t 
          , opt: Token.t 
          , ty: Ty.t 
          , semicolon: Token.t 
          }
    | DeriveAttr of 
          { elem: dec 
          , colon: Token.t 
          , ty: Ty.t 
          , colonequal: Token.t 
          , exp: Exp.t
          , semicolon: Token.t 
          }
    | InverAttr of 
          { elem: dec 
          , colon: Token.t 
          , ty: 
              { container: Token.t 
              , bound: bound option
              , off: Token.t
              } option 
          (* TODO *)
          , attrr: Token.t 
          , forr: Token.t 
          , attre: Token.t 
          , semicolon: Token.t
          }
    | RedecAttr of 
          { attrr1: Token.t 
          , attrr2:
                { renamee: Token.t 
                , attr: Token.t 
                }
          }
    | ReferAttr of Token.t 
    
    and clause =
      WhereClause of 
          { wheree: Token.t 
          , domains: 
                { rule: rule 
                , semicolon: Token.t 
                } list
          }
    | ReferClause of 
          { referr: Token.t 
          , fromm: Token.t 
          , scheman: Token.t 
          , elem: 
              { left: Token.t 
              , elems: Token.t list 
              , delims: Token.t list 
              , right: Token
              } option 
          , semicolon: Token.t 
          }
    | UseClause of 
          { usee: Token.t
          , fromm: Token.t 
          , scheman: Token.t 
          , elem: 
              { left: Token.t 
              , elems: Token.t list 
              , delims: Token.t list 
              , right: Token
              } option 
          , semicolon: Token.t 
          }
    | InverClause of 
          { inversee: Token.t 
          , elems: attr list 
          }
    | UniClause of 
          { uniquee: Token.t
          , elems:
                { rule: rule
                , semicolon: Token.t 
                } list
          }
    | DeriveClause of 
          { derivee: Token.t 
          , elems: attr list
          }

    and alg =
      Alg of 
        { decs: dec list
        , constdec: dec option
        , loccaldec: dec option
        }

    and dec =
      DecEmpty
    | DecFun of
        { funh: Token.t 
        , funn: Token.t 
        , tyvars: Token.t SyntaxSeq.t
        , fvalbind: (alg, stmt) fvalbind
        , funt: Token.t
        }
    | DecProc of
        { proch: Token.t 
        , procn: Token.t
        , tyvars: Token.t SyntaxSeq.t 
        , pvalbind: (alg, stmt) pvalbind
        , proct: Token.t 
        }
    | DecType of
        { typeh: Token.t
        , typen: Token.t 
        , tvalbind: tvalbind
        , typet: Token.t
        }
    | DecEnt of
        { enth: Token.t 
        , entn: Token.t 
        , evalbind: evalbind
        , entt: Token.t
        }
    | DecSub of 
        { subh: Token.t 
        , subn: Token.t 
        , svalbind: svalbind
        , subt: Token.t}
    | DecCons of 
        { conh: Token.t 
        , consb: cvalbind
        , cont: Token.t 
        , semicolon: Token.t 
        }
    | DecRule of 
        { rulh: Token.t  
        , ruln: Token.t 
        , forr: Token.t 
        , left: Token.t 
        , elems: Token.t list 
        , delims: Token.t list 
        , right: Token.t
        , rult: Token.t
        , semicolon: Token.t 
        }
    | DecLoc of 
        { loch: Token.t 
        , elems: 
              { elems: Token.t list  
              , delims: Token.t list
              , colon: Token.t 
              , ty: Ty.t 
              , exp: 
                  { colonequal: Token.t 
                  , exp: Exp.t
                  } option
              , semicolon: Token.t 
              }
        , loct: Token.t 
        , semicolon: Token.t
        }

    and control = 
      IncreCtrl of 
         { incree: Token.t
         , colonequal: Token.t 
         , exp1: Exp.t 
         , to: Token.t
         , exp2: Exp.t
         , by: 
              { by: Token.t 
              , exp: Exp.t  
              } option
         }
    | WhileCtrl of 
         { whilee: Token.t 
         , exp: Exp.t 
         }
    | UntilCtrl of 
         { untill: Token.t 
         , exp: Exp.t
         }

    and rule = 
      RuleDomain of 
          { domainn: 
                { domainn: Token.t 
                , colon: Token.t
                } option
          , exp: Exp.t
          }
    | RuleSuperType of 
          { supertypee: Token.t 
          , subcons: 
                { off: Token.t 
                , left: Token.t 
                , exp: Exp.t
                , right: Token.t 
                }
          }
    | RuleUnique of 
          { uniquee: 
                { uniquee: Token.t 
                , colon: Token.t
                } option
          , elems: attr list 
          , delims: Token.t list 
          }

    and stmt = 
      StmtNull of {semicolon: Token.t}
    | StmtSkip of {skip: Token.t, semicolon: Token.t}
    | StmtEscape of {escape: Token.t, semicolon: Token.t}
    | StmtReturn of 
         { return: Token.t
         , left: Token.t 
         , exp: Exp.t
         , right: Token.t 
         , semicolon: Token.t 
         }
    | StmtCompound of 
         { beginn: Token.t
         , elems: stmt list
         , endd: Token.t
         , semicolon: Token.t 
         }
    | StmtIf of 
         { ifh: Token.t 
         , exp: Exp.t
         , thenn: Token.t 
         , stmts: stmt list
         , elsee:
              { elsee: Token.t 
              , stmts: stmt list
              } option
         , ift: Token.t 
         , semicolon: Token.t 
         }
    | StmtCase of 
         { caseh: Token.t 
         , exp: Exp.t
         , off: Token.t 
         , elems: 
              { elems: Token.t 
              , delims: Token.t 
              , colon: Token.t 
              , stmt: stmt
              } list
         , otherwise: 
              { otherwise: Token.t 
              , colon: Token.t 
              , stmt: stmt
              } option 
         , caset: Token.t 
         , semicolon: Token.t 
         }
    | StmtRepeat of 
         { repeath: Token.t 
         , control: 
              { incree: control option
              , whilee: control option
              , untill: control option
              }
         , semicolon1: Token.t
         , stmts: stmt list
         , repeatt: Token.t 
         , semicolon2: Token.t 
         }
    | StmtAssign of 
         { assignn: Token.t list
         , colonequal: Token.t 
         , exp: Exp.t
         , semicolon: Token.t 
         }
    | StmtAlias of 
         { aliash: Token.t 
         , aliasn: Token.t 
         , forr: Token.t 
         , aliass: Token.t list
         , semicolon1: Token.t 
         , stmts: stmt list 
         , aliast: Token.t 
         , semicolon2: Token.t 
         }
    | StmtProc of 
         { procn: Token.t 
         , pat: Pat.t option
         , semicolon: Token.t 
         }

    structure Schema =
    struct
      type schemabody =
        { inter_spec: clause list
        , cons_dec: dec option
        , rule_dec: dec list
        }
      type schemadec =
        { schemah: Token.t 
        , scheman: 
            { name: Token.t 
            , version: Token.t option
            }
        , semicolon1: Token.t 
        , schemab: schemabody
        , schemat: Token.t 
        , semicolon2: Token.t
        } 

    end

    datatype topdec =
      SchemaDec of Schema.schemadec

    datatype ast = 
      Ast of {topdec: topdec}

  end
end
