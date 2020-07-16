signature RANK =
sig
  datatype 'a rank = @ of int * 'a
  datatype 'a list = NIL | ::: of 'a * 'a list rank
  
  type 'a extra = 'a rank List.list
  type 'a focus = 'a * 'a list rank * 'a extra
  
  val first : 'a list -> 'a focus option
  val next : 'a focus -> 'a focus option
  val all : 'a list -> 'a focus List.list
  
  val collect : 'a list * 'a list -> 'a list * 'a extra
  val rev : 'a extra -> 'a list
end

structure Rank : RANK =
struct
  datatype 'a rank = @ of int * 'a
  datatype 'a list = NIL | ::: of 'a * 'a list rank
  
  type 'a extra = 'a rank List.list
  type 'a focus = 'a * 'a list rank * 'a extra
  
  fun focus (x ::: xs, ys) = SOME (x, xs, ys)
    | focus _ = NONE
  
  fun first xs = focus (xs, nil)
  fun next (x, p @ xs, ys) = focus (xs, (p @ x) :: ys)
  
  fun loop (xs, SOME x) = loop (x :: xs, next x)
    | loop (xs, NONE) = xs
  
  fun all xs = loop (nil, first xs)
  
  fun loop (xs, NIL, zs) = (xs, zs)
    | loop (NIL, ys, zs) = (ys, zs)
    | loop (xxs as x ::: p @ xs, yys as y ::: q @ ys, zs) =
      if p <= q then loop (xs, yys, (p @ x) :: zs)
                else loop (xxs, ys, (q @ y) :: zs)
  
  fun collect (xs, ys) = loop (xs, ys, nil)
  
  fun loop (xs, nil) = xs
    | loop (xs, (p @ x) :: ys) = loop (x ::: p @ xs, ys)
  
  fun rev xs = loop (NIL, xs)
end
