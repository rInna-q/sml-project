local

in
  datatype 'a LinkList = Nil | Cons of 'a * 'a LinkList
  fun rev ( Nil : 'a LinkList ) ( acc : 'a LinkList ) = acc
    | rev ( Cons ( x , next ) ) acc = rev next ( Cons ( x , acc ) )
  fun mergeTwo ( cmp : 'a * 'a -> order ) ( Nil : 'a LinkList ) Nil = Nil
    | mergeTwo cmp Nil L = L 
    | mergeTwo cmp L Nil = L
    | mergeTwo cmp ( Cons ( x , next1 ) ) ( Cons ( y , next2 ) ) =
    case cmp ( x , y ) of 
         GREATER => Cons ( y , mergeTwo cmp ( Cons ( x , next1 ) ) next2 )
       | _ => Cons ( x , mergeTwo cmp next1 (Cons ( y , next2 ) ) )
end

