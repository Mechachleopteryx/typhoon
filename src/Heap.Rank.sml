signature HEAP_RANK =
sig
  include HEAP_BUBBLE
  include RANK
  
  val succ : 'a list -> ('a * 'a some * 'a list rank) option
  val pred : 'a some * 'a list rank -> 'a list rank
end

structure HeapRank2 : HEAP_RANK =
struct
  open Bubble2
  open Rank
  
  fun succ (a ::: p @ b ::: q @ xs) =
      if p = q then SOME (a, b, 1 + p @ xs) else NONE
    | succ _ = NONE
  
  fun pred (a, p @ xs) =
    let val p = p - 1 in p @ a ::: p @ xs end
end

structure HeapRank3 : HEAP_RANK =
struct
  open Bubble3
  open Rank
  
  fun succ (a ::: p @ b ::: q @ c ::: r @ xs) =
      if Equal.all [p, q, r] then SOME (a, (b, c), 1 + p @ xs) else NONE
    | succ _ = NONE
  
  fun pred ((a, b), p @ xs) =
    let val p = p - 1 in p @ a ::: p @ b ::: p @ xs end
end

structure HeapRank4 : HEAP_RANK =
struct
  open Bubble4
  open Rank
  
  fun succ (a ::: p @ b ::: q @ c ::: r @ d ::: s @ xs) =
      if Equal.all [p, q, r, s] then SOME (a, (b, c, d), 1 + p @ xs) else NONE
    | succ _ = NONE
  
  fun pred ((a, b, c), p @ xs) =
    let val p = p - 1 in p @ a ::: p @ b ::: p @ c ::: p @ xs end
end
