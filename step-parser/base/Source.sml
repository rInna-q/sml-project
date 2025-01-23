structure Source :>
sig
  type 'a Seq
  type source 
  type t = source

  val loadFromFile: string -> source

  val make: {fileName: string, contents: char Seq} -> source
  val fileName: source -> string

  val absoluteStartOffset: source -> int
  val absoluteEndOffset: source -> int
  val length: source -> int
  val nth: source -> int -> char
  val slice: source -> int * int -> source
  val drop: source -> int -> source
  
  val toString: source -> string
  val wholeFile: source -> source
  

end =
struct 
  type 'a Seq = 'a ArraySlice.slice
  type source = 
    { data: char Seq
    , fileName: string
    , newlineIdxs: int Seq  
    }
  type t = source

  fun loadFromCharSeq path contents =
    let 
      val n = ArraySlice.length contents

      val (_, absoluteOffset, _) = ArraySlice.base contents
      val _ = 
        if absoluteOffset = 0 then
          ()
        else
          raise Fail "Step File should begin at offset 0."
      val newlineIdxs =
        ArraySlice.full (SeqBasis.filter (0, n) (fn i => i) (fn i =>
        ArraySlice.sub (contents, i) = #"\n"))
    in 
      {data = contents, fileName = path, newlineIdxs = newlineIdxs}
    end 

  fun loadFromFile path = 
    let val contents = ReadFile.contentsSeq (path)
    in loadFromCharSeq path contents
    end

  fun make {fileName, contents} = loadFromCharSeq fileName contents
  fun fileName (s: source) = #fileName s

  fun absoluteStartOffset ({data, ...}: source) =
    let val (_, off, _) = ArraySlice.base data
    in off
    end 

  fun absoluteEndOffset ({data, ...}: source) = 
    let val (_, off, n) = ArraySlice.base data
    in off + n 
    end 

  fun length ({data, ...}: source) = ArraySlice.length data
  fun nth ({data, ...}: source) k = ArraySlice.sub (data, k)
  fun slice {data, fileName, newlineIdxs} (i, len) = 
    { data = ArraySlice.subslice (data, i, SOME len)
    , fileName = fileName
    , newlineIdxs = newlineIdxs
    }

  fun drop s k =
    slice s (k, length s - k)

  fun toString s =
    CharVector.tabulate (length s, nth s)

  fun wholeFile ({data, fileName, newlineIdxs}: source) =
    let 
      val (a, _, _) = ArraySlice.base data
    in 
      {data = ArraySlice.full a, fileName = fileName, newlineIdxs = newlineIdxs}
    end
end

