functor KnomialHeapList (R : HEAP_RANK) :> HEAP_LIST =
struct
  open Compare
  open R
  
  datatype 'a tree = T of 'a * 'a tree some List.list
  
  fun head (T (x, _)) = x
  fun link (T (x, xss), xs) = T (x, xs :: xss)
  
  type 'a list = 'a tree list
  
  val empty = NIL
  fun ops f =
    let
      val comp = pull head f
      val op^ = link o bubble (swap comp)
      
      fun norm xs =
        case succ xs of
            NONE => xs
          | SOME (x, xs, xss) => norm (x ^ xs ::: xss)
      
      fun unstep (p @ x, xs) = x ::: p @ norm xs
      fun restore (xs, ys) = foldl unstep xs ys
      val link = restore o collect
      
      fun restore (T (x, xs), ys, zs) =
        let val _ @ ys = foldl pred ys xs
        in (x, link (ys, rev zs)) end
      
      val comp = pull (head o #1) f
      fun cons (x, xs) = unstep (0 @ T (x, nil), xs)
      fun uncons xs =
        case all xs of
            nil => NONE
          | x :: xs => SOME (restore (foldl (min comp) x xs))
    in
      { cons = cons, link = link, uncons = uncons }
    end
end
