signature COMPARE =
sig
  type 'a compare = 'a * 'a -> bool
  
  val pull : ('a -> 'b) -> 'b compare -> 'a compare
  val min : 'a compare -> 'a * 'a -> 'a
  val max : 'a compare -> 'a * 'a -> 'a
  val swap : 'a compare -> 'a * 'a -> 'a * 'a
end

structure Compare : COMPARE =
struct
  type 'a compare = 'a * 'a -> bool
  
  fun pull g f (x, y) = f (g x, g y)
  fun min op<= (x, y) = if x <= y then x else y
  fun max op<= (x, y) = if x <= y then y else x
  fun swap op<= (x, y) = if x <= y then (x, y) else (y, x)
end
