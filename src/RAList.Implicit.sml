functor ImplicitList (F : FINGER) :> RANDOM_ACCESS_LIST =
struct
  open F
  
  datatype 'a tree = Leaf of 'a | Node of 'a tree many
  
  datatype 'a list
    = Empty
    | Borrow of 'a list ref
    | Carry of 'a tree many * 'a list ref
    | Many of 'a tree finger * 'a list ref
  
  fun split (Many (a, xs)) =
      (case uncons a of
           (x, SOME a) => SOME (x, Many (a, xs))
         | (x, NONE) => SOME (x, Borrow xs))
    | split _ = NONE
  
  fun borrow xs =
    case split xs of
        SOME (Node a, xs) => Many (many a, ref xs)
      | _ => Empty
  
  fun carry (x, Many (a, xs)) =
      (case cons (x, a) of
           (a, SOME x) => Many (a, ref (Carry (x, xs)))
         | (a, NONE) => Many (a, xs))
    | carry (x, _) = Many (pure x, ref Empty)
  
  val empty = Empty
  fun cons (x, xs) = carry (Leaf x, xs)
  
  datatype 'a step = B | C of 'a tree many | M of 'a list ref
  
  fun build (Borrow r, ss) = build (!r, M r :: B :: ss)
    | build (Carry (x, r), ss) = build (!r, M r :: C x :: ss)
    | build args = args
  
  fun break (xs, B :: ss) = break (borrow xs, ss)
    | break (xs, C x :: ss) = break (carry (Node x, xs), ss)
    | break (xs, M r :: ss) = (r := xs; break (xs, ss))
    | break (xs, nil) = xs
  
  fun force xs = break (build (xs, nil))
  fun uncons xs =
    case split xs of
        SOME (Leaf x, xs) => SOME (x, force xs)
      | _ => NONE
  
  fun build (n, p, Borrow r) =
      let val xs = force (!r)
      in r := xs; build (n, p * base, xs) end
    | build (0, _, xs) = xs
    | build (n, p, xs) =
      case split xs of
          NONE => Empty
        | SOME (Leaf _, xs) => build (n - p, p, xs)
        | SOME (Node a, xs) =>
          if n >= p then
            build (n - p, p, xs)
          else
            build (n, p div base, Many (many a, ref xs))
  
  fun break xs =
    case split xs of
        SOME (Node a, xs) => break (Many (many a, ref xs))
      | _ => xs
  
  fun drop (xs, n) = break (build (n, 1, xs))
end
