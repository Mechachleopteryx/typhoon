functor SparseList (R : LIST_RANK) :> RANDOM_ACCESS_LIST =
struct
  open R
  
  datatype 'a tree = Pure of 'a | ^ of 'a tree * 'a tree some
  
  type 'a list = 'a tree list
  
  fun norm xs =
    case carry xs of
        NONE => xs
      | SOME (x, xs, xss) => norm (x ^ xs ::: xss)
  
  val empty = NIL
  fun cons (x, xs) = norm (Pure x ::: 1 @ xs)
  fun uncons NIL = NONE
    | uncons (Pure x ::: _ @ xs) = SOME (x, xs)
    | uncons (x ^ xs ::: xss) = uncons (x ::: borrow (xs, xss))
  
  fun loop (0, xs) = xs
    | loop (_, NIL) = NIL
    | loop (n, Pure _ ::: p @ xs) = loop (n - p, xs)
    | loop (n, x ^ xs ::: p @ xss) =
      if n >= p then
        loop (n - p, xss)
      else
        loop (n, x ::: borrow (xs, p @ xss))
  
  fun drop (xs, n) =
    if n >= 0 then loop (n, xs) else xs
end
