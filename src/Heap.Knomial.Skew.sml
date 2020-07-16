functor SkewKnomialHeapList (R : HEAP_RANK) :> HEAP_LIST =
struct
  open Compare
  open R
  
  datatype 'a tree = T of 'a * 'a List.list * 'a tree some List.list
  
  fun head (T (x, _, _)) = x
  fun link (T (x, xs, ys), y) = T (x, xs, y :: ys)
  
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
      
      fun cons (x, xs) =
        case succ xs of
            NONE => (T (x, nil, nil)) ::: 0 @ xs
          | SOME (xs, xss, xsss) =>
            let val T (y, xs, xss) = xs ^ xss
                val (x, y) = swap f (x, y)
            in (T (x, y :: xs, xss)) ::: xsss end
      
      fun restore (T (x, xs, xss), ys, zs) =
        let val _ @ ys = foldl pred ys xss
            val     ys = foldl cons ys xs
        in (x, link (ys, rev zs)) end
      
      val comp = pull (head o #1) f
      fun uncons xs =
        case all xs of
            nil => NONE
          | x :: xs => SOME (restore (foldl (min comp) x xs))
    in
      { cons = cons, link = link, uncons = uncons }
    end
end
