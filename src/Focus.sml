signature FOCUS =
sig
  type 'a focus = 'a * 'a list * 'a list
  
  val first : 'a list -> 'a focus option
  val next : 'a focus -> 'a focus option
  val all : 'a list -> 'a focus list
end

structure Focus : FOCUS =
struct
  type 'a focus = 'a * 'a list * 'a list
  
  fun focus (x :: xs, ys) = SOME (x, xs, ys)
    | focus _ = NONE
  
  fun first xs = focus (xs, nil)
  fun next (x, xs, ys) = focus (xs, x :: ys)
  
  fun loop (xs, SOME x) = loop (x :: xs, next x)
    | loop (xs, NONE) = xs
  
  fun all xs = loop (nil, first xs)
end
