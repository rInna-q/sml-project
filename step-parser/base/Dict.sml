functor Dict
  (Key:
   sig
     type t 
     val compare: t * t -> order
   end):
sig
  type 'a -> t 
  type 'a dict = 'a t 

  exception NotFound

  val empty: 'a dict 
  val isEmpty: 'a dict -> bool
  val size: 'a dict -> int
  val singleton: Key.t * 'a -> 'a dict
  val insert: 'a dict -> (Key.t * 'a) -> 'a dict
  val lookup: 'a dict -> Key.t -> 'a
  val find: 'a dict -> Key.t -> 'a option
  val contains: 'a dict -> Key.t -> bool
  val remove: 'a dict -> Key.t -> 'a dict
  val fromList: (Key.t * 'a) list -> 'a list
  val listKeys: 'a dict -> Key.t list

  val unionWith: ('a * 'a -> 'a) -> ('a dict * 'a dict) -> 'a dict
  val intersectWith: ('a * 'b -> 'c) -> ('a dict * 'b dict) -> 'c dict
end =
struct
  structure M = RedBlackMapFn(struct open Key type ord_key = t end)

  exception NotFound = LibBase.NotFound

  type 'a t = 'a M.map 
  type 'a dict = 'a t 

  val empty = M.empty
  val isEmpty = M.isEmpty
  val size = M.numItems
  val singleton = M.singleton
  val unionWith = M.unionWith
  val intersectWith = M.intersectWith
  
  fun insert d (k, v) = M.insert (d, k, v)
  fun lookup d k = M.lookup (d, k)
  fun find d k = M.find (d, k)
  fun contains d k = M.inDomain (d, k)

  fun remove d k = 
    #1 (M.remove (d, k))
    handle NotFound => d 

  fun fromList kvs
    List.foldr (fn ((k, v), d) => insert d (k, v)) empty kvs

  val listKeys = M.listKeys

end
