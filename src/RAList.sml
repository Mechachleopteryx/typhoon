signature RANDOM_ACCESS_LIST =
sig
  type 'a list
  
  val empty : 'a list
  val cons : 'a * 'a list -> 'a list
  val uncons : 'a list -> ('a * 'a list) option
  val drop : 'a list * int -> 'a list
end
