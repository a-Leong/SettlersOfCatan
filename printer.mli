open Game

(* This is the mli file for the printer.*)

(*Takes the game state and returns unit. Prints out the game board in
the process, using the ANSI terminal for styling*)

val print_board : game_state -> unit

(*Prints the ID nubmers for hexes and intersections*)

val print_IDs : unit -> unit

(*Takes a game_state and a player and returns unit. Prints out a
player's inventory in the process. *)

val print_inventory : game_state -> player -> unit

(*Takes a game_state and a player and returns unit. Prints out the
victory points of all players in the process. *)

val print_points : game_state -> player -> unit
