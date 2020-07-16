signature UNION_FIND =
sig
  type set
  
  val set : int -> set
  val size : set -> int
  val find : set * int -> int
  val union : set * int * int -> int
end

structure UnionFind :> UNION_FIND =
struct
  open Array
  
  datatype elem = Root of int | Child of int
  
  type set = elem array
  
  fun split (xs, i, j) =
    case sub (xs, j) of
        Root n => (j, n)
      | Child k => (update (xs, i, Child k); split (xs, j, k))
  
  fun root (xs, i) =
    case sub (xs, i) of
        Root n => (i, n)
      | Child j => split (xs, i, j)
  
  fun set n = array (n, Root 0)
  val size = length
  val find = #1 o root
  
  fun union (xs, i, j) =
    let
      val (i, m) = root (xs, i)
      val (j, n) = root (xs, j)
    in
      if i = j then
        i
      else if m < n then
        (update (xs, i, Child j); j)
      else if m > n then
        (update (xs, j, Child i); i)
      else
        (update (xs, j, Child i); update (xs, i, Root (m + 1)); i)
    end
end
