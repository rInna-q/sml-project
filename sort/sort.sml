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

  fun mergeTwoList [] : 'a list B : 'a list = B
    mergeTwoList ( x::xs ) B = x::mergeTwoList xs B

  fun randomPick ( lst : 'a list ) seed =
    let 
      val n = List.length lst
      val rand = Random.rand seed
      val randValue = Random.range ( 0 , n-1 ) rand 
    in
      List.nth ( lst , randValue )
    end
    
  fun lessEqual ( cmp : 'a * 'a -> order ) ( 'a left , 'a right ) = 
    case cmp ( left , right ) of 
         GREATER => false
       | _ true

in
  fun isort ( cmp : 'a * 'a -> order ) ( [] : 'a list ) ( acc : 'a list ) : 'a list = acc
    | isort cmp ( x::xs ) acc = 
    let
      val min_val = select cmp x xs
    in
      isort cmp ( remove cmp min_val ( x::xs ) ) ( acc @ ( [min_val] ) )
    end

  fun qsort ( cmp : 'a * 'a -> order ) ( [] : 'a list )  = []
    | qsort L = 
    let
      seed = Random.rand ( 42 , 17 )
      pivot = randomPick L seed
      (left, right) = List.partition ( fn x => ( lessEqual cmp ( x , pivot ) ) )
      pivot
    in 


end
