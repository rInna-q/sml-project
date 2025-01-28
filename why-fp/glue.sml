local
  datatype 'a lazylist = Nil | Cons of 'a * (unit -> 'a lazylist)
  fun next N x = (x + N / x) / 2.0
  fun repeat f x = Cons (x, fn () => repeat f (f x))
  fun within eps Nil = NONE
    | within eps (Cons (x, thunk)) =
    case thunk () of
      Cons (y, thunk') => 
        if Real.abs (x - y) <= eps then
          SOME y
        else within eps (thunk ()) 
    | Nil => NONE

in
  fun sqrt a0 eps N = within eps (repeat (next N) a0)
end
