structure ExpRestriction =
struct

  datatype t =
    At
  | App
  | Inf
  | None 

  fun appOkay restrict =
    case restrict of
      At => false
    | _ => true

  fun infOkay restrict =
    case restrict of 
      At => false
    | App => false
    | _ => true

  fun anyOkay restrict =
    case restrict of
      None => true
    | _ => false

  fun bumpUp r =
    case r of
      At => App
    | App => Inf
    | Inf => None
    | None => None

end
