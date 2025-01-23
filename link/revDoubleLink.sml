local

in
  datatype 'a DoubleLinkList = Nil | Cons of 'a * 'a DoubleLinkList * 'a DoubleLinkList
  fun appendLeft x Nil = Cons ( x , Nil , Nil )
    | appendLeft x Cons ( y , left , right ) = 
    let node = Cons ( x , Nil , Cons ( y , left , right ) )

  fun rev ( Nil: 'a DoubleLinkList ) ( acc: 'a DoubleLinkList ) = acc
    | rev ( Cons ( x , Nil , next ) ) acc = rev next ( Cons ( x , Nil , acc ) )
end
