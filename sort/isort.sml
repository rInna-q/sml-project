local
  fun select ( cmp : 'a * 'a -> order ) ( x : 'a ) ( [] : 'a list ) : 'a = x
    | select cmp x ( y::L ) = 
    case cmp ( x , y ) of 
         GREATER => select cmp y L 
       | _ => select cmp x L 

  fun remove ( cmp : 'a * 'a -> order ) ( x : 'a ) ( [] : 'a list ) : 'a list = []
    | remove cmp x ( y::L ) = 
    case cmp ( x , y ) of 
         EQUAL => L 
       | _ => y::( remove cmp x L ) 
in
  fun isort ( cmp : 'a * 'a -> order ) ( [] : 'a list ) ( acc : 'a list ) : 'a list = acc
    | isort cmp ( x::xs ) acc = 
    let
      val min_val = select cmp x xs
    in
      isort cmp ( remove cmp min_val ( x::xs ) ) ( acc @ ( [min_val] ) )
    end
end
