signature EQUAL =
sig
  val all : int list -> bool
  val adj : int list -> bool
end

structure Equal : EQUAL =
struct
  fun all (x :: y :: xs) = if x = y then all (y :: xs) else false
    | all _ = true
  
  fun adj (x :: y :: xs) = if x = y then true else adj (y :: xs)
    | adj _ = false
end
