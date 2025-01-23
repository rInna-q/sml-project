structure Error:
sig
  datatype element =
    Pragraph of string
  | ItemList of string list 
  | SourceReference of Source.t 

  type t = {header: string, content: element list}
  type err = t 
  exception Error of err

  val lineError:
    {header: string, pos: Source.t, what: string, explain: string option}
    -> err

end = 
struct
  datatype element = 
    Pragraph of string
  | ItemList of string list
  | SourceReference of Source.t 

  type t = {header: string, content: element list}
  type err = t 
  exception Error of err

  fun lineError {header, pos, what, explain} =
    let 
      val elems = [Pragraph what, SourceReference pos]

      val more = 
        case explain of 
          NONE => []
        | SOME s => [Pragraph s]
    in
      {header = header, content = elems @ more}
    end
end
