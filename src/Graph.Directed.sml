signature DIRECTED_GRAPH =
sig
  type block = int list
  type graph = int list array
  
  val tarjan : graph * block -> unit -> block
  val gabow : graph * block -> unit -> block
end

structure DirectedGraph : DIRECTED_GRAPH =
struct
  open IntArray
  
  type block = int list
  type graph = int list Array.array
  
  fun tarjan (xs, is) =
    let
      val size = Array.length xs
      val slow = array (size, ~1)
      val fast = array (size, ~1)
      val pend = ref nil
      val next = ref 0
      
      fun bump (i, n) =
        (update (slow, i, n);
         update (fast, i, n);
         pend := i :: !pend;
         next := n + 1)
      
      fun adjust (~1, _) = ()
        | adjust (i, n) =
          if sub (fast, i) > n then update (fast, i, n) else ()
      
      fun enter (i, j) =
        case sub (slow, j) of
            ~1 => (bump (j, !next); SOME (Array.sub (xs, j)))
          | n => (adjust (i, n); NONE)
      
      fun split (_, _, nil) = raise Empty
        | split (i, is, j :: js) =
          (update (slow, j, size);
           if i = j then
             (pend := js; i :: is)
           else
             split (i, j :: is, js))
      
      fun leave (_, ~1) = nil
        | leave (i, j) = 
          let
            val m = sub (slow, j)
            val n = sub (fast, j)
          in
            if m = n then
              split (j, nil, !pend)
            else
              (update (slow, j, n); adjust (i, n); nil)
          end
      
      val steps = ref ((~1, ~1, is) :: nil)
      
      fun run nil = nil
        | run ((i, j, nil) :: ss) =
          (steps := ss;
           case leave (i, j) of
               nil => run ss
             | xs => xs)
        | run ((i, j, k :: ks) :: ss) =
          (steps := (i, j, ks) :: ss;
           case enter (j, k) of
               NONE => run ((i, j, ks) :: ss)
             | SOME ls => run ((j, k, ls) :: (i, j, ks) :: ss))
      
      fun resume () = run (!steps)
    in
      resume
    end
  
  fun gabow (xs, is) =
    let
      val size = Array.length xs
      val pend = array (size, ~1)
      val slow = ref nil
      val fast = ref nil
      val next = ref 0
      
      fun bump (i, n) =
        (update (pend, i, n);
         slow := i :: !slow;
         fast := i :: !fast;
         next := n + 1)
      
      fun adjust (_, nil) = ()
        | adjust (n, i :: is) =
          if sub (pend, i) > n then adjust (n, is) else fast := i :: is
      
      fun enter i =
        case sub (pend, i) of
            ~1 => (bump (i, !next); SOME (Array.sub (xs, i)))
          | n => (adjust (n, !fast); NONE)
      
      fun split (_, _, nil) = raise Empty
        | split (i, is, j :: js) =
          (update (pend, j, size);
           if i = j then
             (slow := js; i :: is)
           else
             split (i, j :: is, js))
      
      fun leave (_, nil) = nil
        | leave (i, j :: js) =
          if i = j then
            (fast := js; split (i, nil, !slow))
          else
            nil
      
      val steps = ref ((~1, is) :: nil)
      
      fun run nil = nil
        | run ((i, nil) :: ss) =
          (steps := ss;
           case leave (i, !fast) of
               nil => run ss
             | xs => xs)
        | run ((i, j :: is) :: ss) =
          (steps := (i, is) :: ss;
           case enter j of
               NONE => run ((i, is) :: ss)
             | SOME js => run ((j, js) :: (i, is) :: ss))
      
      fun resume () = run (!steps)
    in
      resume
    end
end
