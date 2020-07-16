signature FINGER =
sig
  type 'a finger
  type 'a many
  
  datatype 'a split = Pure of 'a | Many of 'a finger * 'a finger
  
  val base : int
  val pure : 'a -> 'a finger
  val many : 'a many -> 'a finger
  
  val cons : 'a * 'a finger -> 'a finger * 'a many option
  val snoc : 'a finger * 'a -> 'a many option * 'a finger
  val uncons : 'a finger -> 'a * 'a finger option
  val unsnoc : 'a finger -> 'a finger option * 'a
  
  val split : 'a finger -> 'a split
  val tilps : 'a finger -> 'a split
end

structure Finger123 : FINGER =
struct
  datatype 'a finger
    = One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
  
  type 'a many = 'a * 'a
  
  val base = 2
  val pure = One
  val many = Two
  
  fun cons (a, One b) = (Two (a,b), NONE)
    | cons (a, Two (b,c)) = (Three (a,b,c), NONE)
    | cons (a, Three (b,c,d)) = (Two (a,b), SOME (c,d))
  
  fun snoc (One a, b) = (NONE, Two (a,b))
    | snoc (Two (a,b), c) = (NONE, Three (a,b,c))
    | snoc (Three (a,b,c), d) = (SOME (a,b), Two (c,d))
  
  fun uncons (One a) = (a, NONE)
    | uncons (Two (a,b)) = (a, SOME (One b))
    | uncons (Three (a,b,c)) = (a, SOME (Two (b,c)))
  
  fun unsnoc (One a) = (NONE, a)
    | unsnoc (Two (a,b)) = (SOME (One a), b)
    | unsnoc (Three (a,b,c)) = (SOME (Two (a,b)), c)
  
  datatype 'a split = Pure of 'a | Many of 'a finger * 'a finger
  
  fun split (One a) = Pure a
    | split (Two (a,b)) = Many (One a, One b)
    | split (Three (a,b,c)) = Many (One a, Two (b,c))
  
  fun tilps (One a) = Pure a
    | tilps (Two (a,b)) = Many (One a, One b)
    | tilps (Three (a,b,c)) = Many (Two (a,b), One c)
end
