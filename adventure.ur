(*
urweb adventure
./adventure.exe
http://localhost:8080/Adventure/main
*)

open List

(* Equality of pairs. *)
val eq_pair = fn [a] [b] (_ : eq a) (_ : eq b) =>
    mkEq (fn (x1 : a, y1 : b) (x2 : a, y2 : b) => x1 = x2 && y1 = y2)

(* Showing a pair. *)
val show_pair = fn [a] [b] (_ : show a) (_ : show b) =>
    mkShow (fn (x : a, y : b) => 
        "(" ^ show x ^ ", " ^ show y ^ ")")

(* Updating records. Example: update r { Field1 = value1, Field2 = value2 } *)
(* http://www.impredicative.com/pipermail/ur/2015-March/001901.html *)
fun update [a] [b] [a ~ b] (r : $(a ++ b)) (r' : $b) : $(a ++ b) =
    r --- b ++ r'

(* Various key codes. *)
val keyLeft = 37
val keyUp = 38
val keyRight = 39
val keyDown = 40
val keyReturn = 13

(* The dimensions of the field. *)
val fieldWidth : int = 10
val fieldHeight : int = 10

(* Things the player cannot get. *)
val cannotGet : list string = "friendly gnome" :: "sleeping monster" :: 
                              "hungry monster" :: "wall" :: "door" :: []

type position = int * int
type place = position * list string (* a place is a position and a list of items *)
type field = list place

type gameState = { 
    Stuff : list string, 
    Position : position, 
    PreviousPosition : position,
    Statement : string, 
    Message : string, 
    Field : field }

(* Showing a gameState. *)
val show_gameState = mkShow (fn (gs : gameState) => 
    "gameState{ " ^
    "stuff: " ^ show gs.Stuff ^ 
    ", position: " ^ show gs.Position ^ 
    ", previous position: " ^ show gs.PreviousPosition ^ 
    ", statement: " ^ show gs.Statement ^ 
    ", message: " ^ show gs.Message ^ 
    ", field: " ^ show gs.Field ^
    " }")

(* The initial state of the game. *)
val initializeGame : gameState = { 
    Stuff = [], (* Items in the player's bag. *)
    Position = (0, 0), (* The player's current position. *)
    PreviousPosition = (0, 0),  (* The player's previous position. *)
    Statement = "player's statement", (* The player's currrent utterance. *)
    Message = "game's message", (* A message of the game to the player. *)
    Field = (* The initial game field. *)
        ((1, 0), ("apple" :: "key" :: "goat" :: [])) :: 
        ((2, 0), ("wall" :: [])) :: 
        ((2, 1), ("wall" :: [])) :: 
        ((1, 1), ("wall" :: [])) :: 
        ((0, 1), ("door" :: [])) :: 
        ((0, 2), ("hungry monster" :: [])) :: 
        ((4, 4), ("friendly gnome" :: [])) :: 
        ((fieldWidth - 1, fieldHeight - 1), ("trophy" :: [])) :: 
        []}

(* True iff s starts with part. *)
fun startsWith (s : string) (part : string) : bool = 
    case (strsindex s part) of
    | None => False
    | Some value => value = 0
    
(* Evaluate the current game state and update it according to rules. *)
fun evaluateSituation (gs : gameState) : gameState =
    let
        val previousPlace : list string = lookup' gs.PreviousPosition [] gs.Field
        val place : list string = lookup' gs.Position [] gs.Field
        val stuff = gs.Stuff
        val (x, y) = gs.Position
        val invalidPosition = x < 0 || y < 0 || x >= fieldWidth || y >= fieldHeight
    in
        if place `contains` "wall" || invalidPosition then 
            (* gs -- #Message --#Position 
            ++ {Message = "The door is locked. You cannot pass."}
            ++ {Position = gs.PreviousPosition}*)
            (*gs -- #Message --#Position ++ {
                Message = "There is a wall here. You cannot go through walls.", 
                Position = gs.PreviousPosition }*)
            update gs { 
                Message = "There is a wall here. You cannot go through walls.", 
                Position = gs.PreviousPosition }
        else if place `contains` "door" && not (stuff `contains` "key") then 
            update gs { 
                Message = "The door is locked. You cannot pass.", 
                Position = gs.PreviousPosition }
        else if place `contains` "hungry monster" && place `contains` "apple" then 
            update gs { 
                Message = "The monster eats the apple. It is full and sleeps now.", 
                Field = setPair gs.Position (replace 
                    "hungry monster" "sleeping monster" 
                    (place `remove` "apple")) gs.Field }
        else if previousPlace `contains` "hungry monster" then 
            update gs { 
                Message = "The monster eats you before you can get away.", 
                Position = gs.PreviousPosition }
        else if place `contains` "friendly gnome" && gs.Statement `startsWith` "hello" then 
            update gs { 
                Message = "Hello! I have some secret information for you: " ^ 
                          "The trophy is in the lower right corner of the field!" }
        else if stuff `contains` "trophy" then 
            update gs { Message = "You won! Congratulations!" }
        else gs
    end

(* The player moves by (dx, dy). *)
fun go (gs : gameState) (dx : int, dy : int) : gameState =
    let
        val (x, y) = gs.Position
        val newState = update gs {
            PreviousPosition = (x, y), 
            Position = (x + dx, y + dy),
            Message = "", Statement = "" }
    in
        evaluateSituation newState
    end

(* The player tries to grab an object from the current place. *)
fun processGet (gs : gameState) (object : string) : gameState =
    let
        val place : list string = lookup' gs.Position [] gs.Field
        val newState : gameState =
            if cannotGet `contains` object then 
                update gs { Message = "You cannot get the " ^ object ^ "." }
            else
                if place `contains` object then
                    update gs {
                        Stuff = object :: gs.Stuff, 
                        Field = setPair gs.Position (place `remove` object) gs.Field }
                else
                    update gs { Message = "No " ^ object ^ " here." }
    in
        evaluateSituation newState
    end
    
(* The player tries to drop an object in the current place. *)
fun processPut (gs : gameState) (object : string) : gameState =
    let
        val place : list string = lookup' gs.Position [] gs.Field
        val newState : gameState = 
            if gs.Stuff `contains` object then
                update gs {
                    Stuff = gs.Stuff `remove` object, 
                    Field = setPair gs.Position (object :: place) gs.Field }
            else
                update gs { Message = "No " ^ object ^ " in your bag." }
    in
        evaluateSituation newState
    end

(* The player says something. *)    
fun processSay (gs : gameState) (statement : string) : gameState =
    evaluateSituation (update gs {Statement = statement})

(* Produce a list of buttons from a list of button labels. *)
fun toButtons (sgs : source gameState) (labels : list string) (clicked : gameState -> string -> gameState) : xbody =
let
    fun loop (labels : list string) : xbody = 
        case labels of
        | [] => <xml/>
        | label :: labels => <xml><li>
            <button 
                value={show label} 
                onclick={fn _ => 
                    gs <- get sgs; 
                    set sgs (clicked gs label)}/>
            </li>{loop labels}</xml> 
in
         <xml><ul>{loop labels}</ul></xml> 
end

(* Render the game state as XML/HTML. *)
fun render (sgs : source gameState) : signal xbody =
    gs <- signal sgs;
    return let
        fun cols (row : int) (col : int) : xtr = 
            let
                val playerIsHere : bool = (gs.Position = (col, row))
                val stuffHere : list string = lookup' (col, row) [] gs.Field
                val content : string = 
                    if playerIsHere then "P" 
                    else case stuffHere of 
                    | [] => ""
                    | _ => "x"
            in
                if col < fieldWidth - 1
                then (<xml><td>{[content]}</td>{cols row (col + 1)}</xml>)
                else (<xml><td>{[content]}</td></xml>)
            end
        fun rows (row : int) : xtable = 
            if row < fieldHeight - 1
            then (<xml><tr>{cols row 0}</tr>{rows (row + 1)}</xml>)
            else (<xml><tr>{cols row 0}</tr></xml>)
        val stuffHere : list string = lookup' gs.Position [] gs.Field
    in
        <xml>
        <table border={1}>{rows 0}</table>
        <br/>
        <button value="Left"  onclick={fn _ => set sgs (go gs (-1, 0))}/>
        <button value="Right" onclick={fn _ => set sgs (go gs (1, 0))}/>
        <button value="Up"    onclick={fn _ => set sgs (go gs (0, -1))}/>
        <button value="Down"  onclick={fn _ => set sgs (go gs (0, 1))}/>
        <br/>
        <p>Message: {[gs.Message]}</p>
        <p>Here is:</p> {case stuffHere of 
            | [] => <xml>Nothing</xml>
            | _ => toButtons sgs stuffHere processGet}
        <p>You have:</p> {case gs.Stuff of 
            | [] => <xml>Nothing</xml>
            | _ => toButtons sgs gs.Stuff processPut}
        <p>You say: {[gs.Statement]}</p>
        </xml> 
    end

(* Mapping of key codes to directions. *)
val directions : list (int * position) = 
    (keyLeft, (-1, 0)) :: 
    (keyRight, (1, 0)) :: 
    (keyUp,    (0,-1)) :: 
    (keyDown,  (0, 1)) :: []

(* Entry point of the program. *)
fun main () : transaction page =
    state <- source initializeGame;
    input <- source initializeGame.Statement;
    return <xml>
        <head>
            <title>Adventure</title>
            <link rel="stylesheet" type="text/css" href="/Adventure/style.css"/>
        </head>
        <body
        onkeydown={fn ev => (*alert ("Code " ^ show ev.KeyCode) }*)
            s <- get state; 
            (*debug (show ev.KeyCode)*)
            case lookup (ev.KeyCode + naughtyDebug (show ev.KeyCode)) directions of
                | None => return ()
                | Some direction => set state (go s direction)}>
        <dyn signal={render state} />
        <ctextbox source={input} onkeypress={fn ev => 
            if ev.KeyCode = keyReturn then
                s <- get state; 
                t <- get input;
                set state (processSay s t)
            else return ()}/>
        <button value="Submit" onclick={fn _ => 
            s <- get state; 
            t <- get input;
            set state (processSay s t)} />
        </body></xml>
