structure ST = 
struct 
  type segment = 
    { arr: int Array.array
    , sum: int Array.array
    , add: int Array.array
    }
  type t = segment

  fun update arr i v = Array.update (arr, i, v)
  fun sub arr i = Array.sub (arr, i)

  infix 7 <<
  fun x << y = Word.toInt (Word.<< (Word.fromInt x, Word.fromInt y))
  infix 7 >>
  fun x >> y = Word.toInt (Word.>> (Word.fromInt x, Word.fromInt y))

  infix 6 ||
  fun x || y = Word.toInt (Word.orb (Word.fromInt x, Word.fromInt y))

  fun fromList (l: int list) = 
    let
      val arr = Array.fromList l 
      val sum = Array.array ((Array.length arr) * 4, 0)
      val add = Array.array ((Array.length arr) * 4, 0)
    in 
      {arr = arr, sum = sum, add = add}
    end

  fun lazy' {sum, add} i v n = 
    let
      val _ = update sum i (sub sum i + v * n)
      val _ = update add i (sub add i + v)
    in 
      ()
    end

  fun up sum i = 
    let 
      val new = sub sum (i << 1) + sub sum (i << 1 || 1)
    in 
      update sum i new
    end

  fun down {sum, add} i ln rn =
    let 
      val v = sub add i
    in 
      if v <> 0 then 
        let
          val _ = lazy' {sum = sum, add = add} (i << 1) v ln
          val _ = lazy' {sum = sum, add = add} (i << 1 || 1) v rn
        in 
          update add i 0
        end
      else
        ()
    end

  fun build (seg as {arr, sum, add}) l r i =
    let
      val _ = 
        if l = r then
          update sum i (sub arr l)
        else
          let
            val mid = (l + r) >> 1  
            val _ = build seg l mid (i << 1)
            val _ = build seg (mid + 1) r (i << 1 || 1)
          in 
            up sum i  
          end
    in 
      update add i 0
    end

  fun add' {sum, add} jobl jobr jobv l r i =
    if jobl <= l andalso r <= jobr then 
      lazy' {sum = sum, add = add} i jobv (r - l + 1)
    else
      let 
        val mid = (l + r) >> 1 
        val _ = down {sum = sum, add = add} i (mid - l + 1) (r - mid)
      in 
        let
          val _ = 
            if jobl <= mid then
              add' {sum = sum, add = add} jobl jobr jobv l mid (i << 1) 
            else
              add' {sum = sum, add = add} jobl jobr jobv (mid + 1) r (i << 1 || 1)
        in 
          up sum i 
        end
      end

  fun query (seg: t) jobl jobr l r i =
    if jobl <= l andalso r <= jobr then
      sub (#sum seg) i
    else
      let 
        val mid = (l + r) >> 1  
        val _ = down {sum = #sum seg, add = #add seg} i (mid - l + 1) (r - mid)
        val ans = 0 
      in 
        if jobl <= mid then
          ans + query seg jobl jobr l mid (i << 1)
        else
          ans + query seg jobl jobr (mid + 1) r (i << 1 || 1)
      end
end
