local

in
  datatype 'a LinkList = Nil | Cons of 'a * 'a LinkList
  fun rev (Nil: 'a LinkList) (acc : 'a LinkList) = acc
    | rev ( Cons ( x , next ) ) acc = rev next ( Cons ( x , acc ) )
end

