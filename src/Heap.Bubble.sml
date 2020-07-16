signature HEAP_BUBBLE =
sig
  type 'a some
  
  val bubble : ('a * 'a -> 'a * 'a) -> 'a * 'a some -> 'a * 'a some
end

structure Bubble2 : HEAP_BUBBLE =
struct
  type 'a some = 'a
  
  fun bubble f = f
end

structure Bubble3 : HEAP_BUBBLE =
struct
  type 'a some = 'a * 'a
  
  fun bubble op^ (a, (b, c)) =
    let val (a, b) = a ^ b
        val (a, c) = a ^ c
    in (a, (b, c)) end
end

structure Bubble4 : HEAP_BUBBLE =
struct
  type 'a some = 'a * 'a * 'a
  
  fun bubble op^ (a, (b, c, d)) =
    let val (a, b) = a ^ b
        val (a, c) = a ^ c
        val (a, d) = a ^ d
    in (a, (b, c, d)) end
end
