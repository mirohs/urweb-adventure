val length : a ::: Type -> list a -> int
val ith : a ::: Type -> int -> list a -> option a
val reverse : a ::: Type -> list a -> list a
val contains : a ::: Type -> eq a -> list a -> a -> bool
val containedIn : a ::: Type -> eq a -> a -> list a -> bool
val filter : a ::: Type -> (a -> bool) -> list a -> list a
val exists : a ::: Type -> (a -> bool) -> list a -> bool
val find : a ::: Type -> (a -> bool) -> list a -> option a
val remove : a ::: Type -> eq a -> list a -> a -> list a
val removeFrom : a ::: Type -> eq a -> a -> list a -> list a
val removeAll : a ::: Type -> eq a -> list a -> a -> list a
val removeAllFrom : a ::: Type -> eq a -> a -> list a -> list a
val replace : a ::: Type -> eq a -> a -> a -> list a -> list a
val toString : a ::: Type -> show a -> list a -> string
(*val show : a ::: Type -> show a -> show (list a)*)
val show_list : a ::: Type -> show a -> show (list a)
val toXml : a ::: Type -> show a -> list a -> xbody

val lookup : a ::: Type -> b ::: Type -> eq a -> a -> list (a * b) -> option b
val lookup' : a ::: Type -> b ::: Type -> eq a -> a -> b -> list (a * b) -> b
val removePair : a ::: Type -> b ::: Type -> eq a -> a -> list (a * b) -> list (a * b)
val setPair : a ::: Type -> b ::: Type -> eq a -> a -> b -> list (a * b) -> list (a * b)
