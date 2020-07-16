signature ATOM_DEQUE =
sig
  type 'a deque
  
  val empty : 'a deque
  val cons : 'a * 'a deque -> 'a deque
  val snoc : 'a deque * 'a -> 'a deque
  val uncons : 'a deque -> ('a * 'a deque) option
  val unsnoc : 'a deque -> ('a deque * 'a) option
end

signature BULK_DEQUE =
sig
  include ATOM_DEQUE
  
  val link : 'a deque * 'a deque -> 'a deque
end
