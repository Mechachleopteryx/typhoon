signature LIST_RANK =
sig
  include RANK
  
  type 'a some
  
  val carry : 'a list -> ('a * 'a some * 'a list rank) option
  val borrow : 'a some * 'a list rank -> 'a list rank
end

structure ListRank2 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a
  
  fun carry (a ::: p @ b ::: q @ xs) =
      if p = q then SOME (a, b, p + q @ xs) else NONE
    | carry _ = NONE
  
  fun borrow (a, p @ xs) =
    let val p = p div 2 in p @ a ::: p @ xs end
end

structure ListRank3 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a * 'a
  
  fun carry (a ::: p @ b ::: q @ c ::: r @ xs) =
      if Equal.all [p, q, r] then SOME (a, (b, c), p + q + r @ xs) else NONE
    | carry _ = NONE
  
  fun borrow ((a, b), p @ xs) =
    let val p = p div 3 in p @ a ::: p @ b ::: p @ xs end
end

structure ListRank4 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a * 'a * 'a
  
  fun carry (a ::: p @ b ::: q @ c ::: r @ d ::: s @ xs) =
      if Equal.all [p, q, r, s] then SOME (a, (b, c, d), p + q + r + s @ xs) else NONE
    | carry _ = NONE
  
  fun borrow ((a, b, c), p @ xs) =
    let val p = p div 4 in p @ a ::: p @ b ::: p @ c ::: p @ xs end
end

structure ListRankSkew2 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a * 'a
  
  fun carry (a ::: 1 @ b ::: p @ c ::: q @ xs) =
      if p = q then SOME (a, (b, c), 1 + p + q @ xs) else NONE
    | carry _ = NONE
  
  fun borrow ((a, b), p @ xs) =
    let val p = p div 2 in 1 @ a ::: p @ b ::: p @ xs end
end

structure ListRankSkew3 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a * 'a * 'a
  
  fun carry (a ::: 1 @ b ::: p @ c ::: q @ d ::: r @ xs) =
      if Equal.all [p, q, r] then SOME (a, (b, c, d), 1 + p + q + r @ xs) else NONE
    | carry _ = NONE
  
  fun borrow ((a, b, c), p @ xs) =
    let val p = p div 3 in 1 @ a ::: p @ b ::: p @ c ::: p @ xs end
end

structure ListRankSkew4 : LIST_RANK =
struct
  open Rank
  
  type 'a some = 'a * 'a * 'a * 'a
  
  fun carry (a ::: 1 @ b ::: p @ c ::: q @ d ::: r @ e ::: s @ xs) =
      if Equal.all [p, q, r, s] then SOME (a, (b, c, d, e), 1 + p + q + r + s @ xs) else NONE
    | carry _ = NONE
  
  fun borrow ((a, b, c, d), p @ xs) =
    let val p = p div 4 in 1 @ a ::: p @ b ::: p @ c ::: p @ d ::: p @ xs end
end
