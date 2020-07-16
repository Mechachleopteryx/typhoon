functor ImplicitDeque (F : FINGER) :> ATOM_DEQUE =
struct
  open F
  
  datatype 'a frame
    = Empty
    | Pure of 'a
    | Cons of 'a many * 'a frame ref
    | Snoc of 'a frame ref * 'a many
    | Uncons of 'a frame ref * 'a finger
    | Unsnoc of 'a finger * 'a frame ref
    | Many of 'a finger * 'a frame ref * 'a finger
  
  fun pair (x, y) = Many (pure x, ref Empty, pure y)
  fun few (F.Pure x) = Pure x
    | few (F.Many (a, b)) = Many (a, ref Empty, b)
  
  fun consT (x, Many (a, xs, b)) =
      (case cons (x, a) of
           (a, SOME x) => Many (a, ref (Cons (x, xs)), b)
         | (a, NONE) => Many (a, xs, b))
    | consT (x, Pure y) = pair (x, y)
    | consT (x, _) = Pure x
  
  fun snocT (Many (a, xs, b), x) =
      (case snoc (b, x) of
           (SOME x, b) => Many (a, ref (Snoc (xs, x)), b)
         | (NONE, b) => Many (a, xs, b))
    | snocT (Pure x, y) = pair (x, y)
    | snocT (_, x) = Pure x
  
  fun unconsT (Many (a, xs, b)) =
      (case uncons a of
           (x, SOME a) => SOME (x, Many (a, xs, b))
         | (x, NONE) => SOME (x, Uncons (xs, b)))
    | unconsT (Pure x) = SOME (x, Empty)
    | unconsT _ = NONE
  
  fun unsnocT (Many (a, xs, b)) =
      (case unsnoc b of
           (SOME b, x) => SOME (Many (a, xs, b), x)
         | (NONE, x) => SOME (Unsnoc (a, xs), x))
    | unsnocT (Pure x) = SOME (Empty, x)
    | unsnocT _ = NONE
  
  datatype 'a tree = Leaf of 'a | Node of 'a tree many
  
  fun uncons (xs, b) =
    case unconsT xs of
        SOME (Node a, xs) => Many (many a, ref xs, b)
      | _ => few (split b)
  
  fun unsnoc (a, xs) =
    case unsnocT xs of
        SOME (xs, Node b) => Many (a, ref xs, many b)
      | _ => few (tilps a)
  
  type 'a deque = 'a tree frame
  
  val empty = Empty
  fun cons (x, xs) = consT (Leaf x, xs)
  fun snoc (xs, x) = snocT (xs, Leaf x)
  
  datatype 'a step
    = C of 'a many
    | S of 'a many
    | Uc of 'a finger
    | Us of 'a finger
    | M of 'a frame ref
  
  fun build (Cons (x, r), ss) = build (!r, M r :: C x :: ss)
    | build (Snoc (r, x), ss) = build (!r, M r :: S x :: ss)
    | build (Uncons (r, x), ss) = build (!r, M r :: Uc x :: ss)
    | build (Unsnoc (x, r), ss) = build (!r, M r :: Us x :: ss)
    | build args = args
  
  fun break (xs, C x :: ss) = break (consT (Node x, xs), ss)
    | break (xs, S x :: ss) = break (snocT (xs, Node x), ss)
    | break (xs, Uc x :: ss) = break (uncons (xs, x), ss)
    | break (xs, Us x :: ss) = break (unsnoc (x, xs), ss)
    | break (xs, M r :: ss) = (r := xs; break (xs, ss))
    | break (xs, nil) = xs
  
  fun force xs = break (build (xs, nil))
  
  fun uncons xs =
    case unconsT xs of
        SOME (Leaf x, xs) => SOME (x, force xs)
      | _ => NONE
  
  fun unsnoc xs =
    case unsnocT xs of
        SOME (xs, Leaf x) => SOME (force xs, x)
      | _ => NONE
end
