fun length [a] (ls : list a) : int =
    let
        fun loop acc ls =
            case ls of
            | [] => acc
            | x :: ls => loop (acc + 1) ls
    in
        loop 0 ls
    end

fun ith [a] (i : int) (ls : list a) : option a =
    let
        fun loop j ls =
            case ls of
            | [] => None
            | x :: ls => if i = j then Some x else loop (j + 1) ls
    in
      loop 0 ls
    end

fun reverse [a] (ls : list a) : list a =
    let
        fun loop acc ls =
            case ls of
            | [] => acc
            | x :: ls => loop (x :: acc) ls
    in
        loop [] ls
    end

fun contains [a] (_ : eq a) (ls : list a) (item : a) : bool =
    case ls of
    | [] => False
    | x :: ls => if item = x then True else contains ls item

fun containedIn [a] (_ : eq a) (item : a) (ls : list a) : bool =
    case ls of
    | [] => False
    | x :: ls => if item = x then True else containedIn item ls

fun exists [a] (f : a -> bool) (ls : list a)  : bool =
    case ls of
    | [] => False
    | x :: ls => if f x then True else exists f ls

fun forall [a] (f : a -> bool) (ls : list a) : bool =
    case ls of
    | [] => True
    | x :: ls => if f x then forall f ls else False


fun find [a] (f : a -> bool) (ls : list a) : option a =
    case ls of
    | [] => None
    | x :: ls => if f x then Some x else find f ls

fun filter [a] (f : a -> bool) (ls : list a) : list a =
    let
        fun loop acc ls =
            case ls of
            | [] => reverse acc
            | x :: ls => loop (if f x then x :: acc else acc) ls
    in
        loop [] ls
    end

fun remove [a] (_ : eq a) (ls : list a) (item : a) : list a =
    case ls of
    | [] => []
    | x :: ls => if x = item then ls else x :: remove ls item

fun removeFrom [a] (_ : eq a) (item : a) (ls : list a) : list a =
    case ls of
    | [] => []
    | x :: ls => if x = item then ls else x :: removeFrom item ls

fun removeAll [a] (_ : eq a) (ls : list a) (item : a) : list a =
    let
        fun loop acc ls =
            case ls of
            | [] => reverse acc
            | x :: ls => loop (if x = item then acc else x :: acc) ls
    in
        loop [] ls
    end

fun removeAllFrom [a] (_ : eq a) (item : a) (ls : list a) : list a =
    let
        fun loop acc ls =
            case ls of
            | [] => reverse acc
            | x :: ls => loop (if x = item then acc else x :: acc) ls
    in
        loop [] ls
    end

fun replace [a] (_ : eq a) (x : a) (x' : a) (ls : list a) : list a = 
    let
        fun loop acc ls =
            case ls of
            | [] => reverse acc
            | y :: ls => loop ((if x = y then x' else y) :: acc) ls
    in
        loop [] ls
    end
    
fun toString [a] (_ : show a) (ls : list a) : string =
    let
        fun loop acc ls =
            case ls of
            | [] => acc ^ "]"
            | x :: [] => acc ^ show x ^ "]"
            | x :: ls => loop (acc ^ show x ^ ", ") ls
    in
        loop "[" ls
    end

val show_list = fn [a] (_ : show a)  =>
    let
        fun loop acc ls =
            case ls of
            | [] => acc ^ "]"
            | x :: [] => acc ^ show x ^ "]"
            | x :: ls => loop (acc ^ show x ^ ", ") ls
    in
        mkShow (loop "[")
    end

fun toXml [a] (_ : show a) (ls : list a) : xbody =
    let
        fun loop ls = 
            case ls of
            | [] => <xml/>
            | x :: ls => <xml><li>{[x]}</li>{loop ls}</xml> 
    in
        <xml><ul>{loop ls}</ul></xml> 
    end

(* association lists *)

fun lookup [a] [b] (_ : eq a) (key : a) (ls : list (a * b)) : option b = 
    case ls of
    | [] => None
    | (key', value) :: ls => 
        if key = key' then Some value else lookup key ls

fun lookup' [a] [b] (_ : eq a) (key : a) (default : b) (ls : list (a * b)) : b = 
    case ls of
    | [] => default
    | (key', value) :: ls => 
        if key = key' then value else lookup' key default ls

fun removePair [a] [b] (_ : eq a) (key : a) (ls : list (a * b)) : list (a * b) = 
    let
        fun loop acc ls =
            case ls of
            | [] => reverse acc
            | (key', value) :: ls => 
                loop (if key = key' then acc else (key', value) :: acc) ls
    in
        loop [] ls
    end

fun setPair [a] [b] (_ : eq a) (key : a) (value : b) (ls : list (a * b)) : list (a * b) = 
    let
        fun loop acc found ls =
            case ls of
            | [] => if found then reverse acc
                    else (key, value) :: reverse acc
            | (key', value') :: ls => 
                loop ((key', if key = key' then value else value') :: acc) 
                     (found || key = key') 
                     ls
    in
        loop [] False ls
    end

