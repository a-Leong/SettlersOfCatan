(* Code written by Alex Leong *)

type resource = Brick | Lumber | Ore | Wheat | Sheep | Desert

type devcard = Knight | Victory_pt | Road_building | Year_of_plenty | Monopoly

type settlement_t = Town | City

type p_color = Red | Blue | Green | Yellow 

type port = Port of resource | Wildcard

type road = intersection * intersection

and settlement = {
	mutable settle_type: settlement_t;
	owner: p_color;
	location: intersection;
}

and intersection = {
	id : int;
	mutable settlement: settlement option;
	mutable port: port option;
	mutable adj_hexes: hex list;
	mutable adj_intersects: int list;
}

and hex = {
	h_id: int;
	h_resource: resource;
	mutable h_robber_pres: bool;
	h_intersections: int list;
	h_roll_number: int;
	h_roll_frequency: int;
}

type board = {
	hex_tiles: hex list;
	mutable intersections: intersection list;
	mutable robber_location: hex;
	mutable roads: road list;
}

type player = {
	p_color: p_color;
	p_ai: bool;
	mutable p_inventory: resource list;
	mutable p_victorypts: int;
	mutable p_devcards: devcard list;
	mutable p_knights: int;
	mutable p_ports: port list;
	mutable p_settled: int list;
	mutable p_start_road1: road option;
	mutable p_start_road2: road option;
	mutable p_roads: road list;
	mutable p_road_supply: int;
	mutable p_town_supply: int;
	mutable p_city_supply: int;
}

type game_state = {
	mutable curr_board: board;
	mutable curr_player: player;
	players: player list;
	mutable devcard_pile: devcard list;
	mutable victory_pt_limit: int;
	mutable longest_road: player option;
	mutable largest_army: player option;
}

type move = 		
			| M_DevCard 
			| M_Road of (intersection * intersection)
			| M_Town of intersection 
			| M_City of intersection 
			| M_Trade of (resource * resource)
			| M_Port 
			| M_EndTurn

val interp_ai		: move -> game_state -> unit

val interp_ai_setup	: move * move -> game_state -> unit

(* Returns the string name of a resource. *)
val string_of_r 	: resource -> string

(* Returns a string of resources. E.g. "1 brick, 3 lumber, 2 ore". *)
val string_of_rl 	: resource list -> string

(* Returns the string name of a player from a player's color. *)
val string_of_pc 	: p_color -> string

(* Returns the string name of a development card. *)
val string_of_d 	: devcard -> string

(* Returns a string of development cards. E.g. "3 Knight, 1 Road_building". *)
val string_of_dl 	: devcard list -> string

(* Returns the string description of a development card. *)
val dc_description 	: devcard -> string

(* Returns a fully initialized, randomized game_state record. *)
val game_init		: unit -> game_state

(* Returns an int in {2...12} *)
val roll_dice		: unit -> int

(*  Takes dice number, updates the game_state so that each player recieves the
 *  appropriate number of each resource according to their settlements
 *  (1 for each town and 2 for each city adjacent to rolled hexes) *)
val collect_r		: int -> game_state -> unit

(* Takes in two intersection IDs and a game state, updates game_state to reflect
 * the newly built road if legal, else raises exception Cannot_build *)
val build_road		: int -> int -> game_state -> unit

(* Takes an intersection ID and a game state, updates game_state to reflect
 * the newly built town/city if legal, else raises exception Cannot_build *)
val build_town		: int -> game_state -> unit
val build_city		: int -> game_state -> unit

(* Takes two intersection IDs, places a town/city at specified intersection if
 * legal, else raises exception Cannot_build
 * Like build_road, except no resources are expended and the road must be
 * adjacent to the player's most recently setup town. *)
val setup_road 		: int -> int -> game_state -> unit

(* Takes an intersection ID and a game state, updates game_state to reflect
 * the newly built town if legal, else raises exception Cannot_build.
 * Like build_town, except no resources are expended and the town doesn't
 * have to be connected to the current player's road system. *)
val setup_town		: int -> game_state -> unit

(* Builds a development card, places it in player's devcard inventory. If
 * insufficient resources, raises Cannot_build *)
val build_devcard	: game_state -> unit

(* true if possible for current player to perform a bank trade.*)
val trade_possible  : game_state -> bool

(* true if possible for current player to build a road.*)
val road_possible  : game_state -> bool

(* true if possible for current player to build a town.*)
val town_possible  : game_state -> bool

(* true if possible for current player to build a city.*)
val city_possible  : game_state -> bool

(* Returns the intersection record associated with an id *)
val i_from_id 		: intersection list -> int -> intersection

val play_knight 	: int -> p_color -> game_state -> unit

val play_road_building	: int -> int -> game_state -> unit

val play_YOP 		: resource -> resource -> game_state -> unit

val setup_collect	: game_state -> unit

val rob 			: game_state -> unit

val play_monopoly 	: resource -> game_state -> unit

(* Remove four of some resource for one resource of any type.
 * Returns unit, updates game_state to reflect trade if possible,
 * raises exception Cannot_trade if impossible. *)
val bank_trade		: resource -> resource -> game_state -> unit

(* Moves robber to specified hex tile *)
val move_robber		: hex -> game_state -> unit

(* Removes one random resource from specified player and adds that resource
 * to the current player's inventory. Does nothing if victim's inventory
 * is empty. *)
val steal_from		: p_color -> game_state -> unit

(* Removes chosen cards from a player inventory *)
val discard_cards	: resource list -> p_color -> game_state -> unit

(* runs after each instance of knight being played or a road being built*)
val update_trophy	: game_state -> unit

(* Updates the number of victory points required to win. *)
val change_victory_pt_limit : int -> game_state -> unit

(* Updates curr_player field*)
val end_turn		: game_state -> unit 

(* Checks to see if the current player has reached or exceeded the victory
 * point limit. If they have, prints a victory message, else returns unit. *)
val check_win 		: game_state -> unit
