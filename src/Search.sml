signature SEARCH_TREE =
sig
  type 'a tree
  type 'a leaf
  type 'a hole
  
  datatype 'a focus = Leaf of 'a leaf | Node of 'a * 'a hole
  
  val empty : 'a tree
  
  val root : 'a tree -> 'a focus
  val left : 'a * 'a hole -> 'a focus
  val right : 'a * 'a hole -> 'a focus
  
  val insert : 'a * 'a leaf -> 'a tree
  val update : 'a * 'a hole -> 'a tree
  val delete : 'a hole -> 'a tree
  val splay : 'a leaf -> 'a tree
  
  val fromAsc : 'a list -> 'a tree
  val fromDesc : 'a list -> 'a tree
end

signature SEARCH_TREE_EX =
sig
  include SEARCH_TREE
  
  val lookup : ('a -> order) -> 'a focus -> 'a focus
  val modify : ('a option -> 'a option) -> 'a focus -> 'a tree
  
  val toAsc : 'a tree -> 'a list
  val toDesc : 'a tree -> 'a list
end

functor SearchTreeEx (T : SEARCH_TREE) : SEARCH_TREE_EX =
struct
  open T
  
  fun lookup _ (Leaf xs) = Leaf xs
    | lookup f (Node xs) =
      case f (#1 xs) of
          LESS => lookup f (left xs)
        | EQUAL => Node xs
        | GREATER => lookup f (right xs)
  
  fun leaf (NONE, xs) = splay xs
    | leaf (SOME x, xs) = insert (x, xs)
  
  fun hole (NONE, xs) = delete xs
    | hole (SOME x, xs) = update (x, xs)
  
  fun modify f (Leaf xs) = leaf (f NONE, xs)
    | modify f (Node (x, xs)) = hole (f (SOME x), xs)
  
  datatype 'a step = One of 'a | Many of 'a * 'a hole
  
  fun (Leaf _) ::: ss = ss
    | (Node xs) ::: ss = Many xs :: ss
  
  fun loop (xs, nil) = xs
    | loop (xs, One x :: ss) = loop (x :: xs, ss)
    | loop (xs, Many ys :: ss) = loop (xs, right ys ::: One (#1 ys) :: left ys ::: ss)
  
  fun toAsc xs = loop (nil, root xs ::: nil)
  
  fun loop (xs, nil) = xs
    | loop (xs, One x :: ss) = loop (x :: xs, ss)
    | loop (xs, Many ys :: ss) = loop (xs, left ys ::: One (#1 ys) :: right ys ::: ss)
  
  fun toDesc xs = loop (nil, root xs ::: nil)
end
