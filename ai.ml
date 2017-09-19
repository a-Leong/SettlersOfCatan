(* Code written by Beau Mahadev *)

open Game 

type premove = 
			| PM_DevCard 
			| PM_Road 
			| PM_Town 
			| PM_City 
			| PM_Trade 
			| PM_Port 
			| PM_EndTurn

let rec in_list (e:'a) (lst: 'a list) = 
	match lst with 
	|h::t -> if h=e then true else in_list e t 
	|[]   -> false

let rec road_in_list (rd: intersection * intersection) (rdlst: road list)=
	match rdlst with 
	|h::t -> if ((fst(rd)).id = (fst(h)).id || (fst(rd)).id = (snd(h)).id) &&
				((snd(rd)).id = (fst(h)).id || (snd(rd)).id = (snd(h)).id ) then 
				true else road_in_list rd t 
	|[]	  -> false 

(*True if this intersection is connected to this road list*)
let rec int_in_list i ilst = 
	match ilst with 
	|(e1,e2)::t -> if e1.id=i.id || e2.id=i.id then true else int_in_list i t 
	| []		-> false

(*True if this intersection is connected to this road list*)
let rec int_in_intlist i ilst = 
	match ilst with 
	|h::t -> if h.id = i.id then true else int_in_intlist i t 
	| []		-> false

(*Check for duplicates before adding to a list*)
let rec add_no_dup e lst =
	if in_list e lst then lst else e::lst 


(*Returns max element in the list via the built in compare > function*)	 
let get_max lst = 
match lst with
	|[] -> failwith "None"
	|h::t ->  let rec helper (x,y) =
	              match y with 
	              [] -> x
	              |h'::t' -> let x' = if h' > x then h' else x in 
	                         let y' = t'
	              in helper (x',y')
	            in helper (h,t)

(*Returns max element in the list via the built in compare > function, but only considers the first element*)	 
let get_max_fst lst = 
match lst with
	|[] -> failwith "None"
	|h::t ->  let rec helper (x,y) =
	              match y with 
	              [] -> x
	              |h'::t' -> let x' = if fst(h') > fst(x) then h' else x in 
	                         let y' = t'
	              in helper (x',y')
	            in helper (h,t) 	            

(*Returns max element in the list via the built in compare > function, 
element must also satisfy f*)	 
let get_max_p (lst: 'a list) (f: 'a -> bool)= 
match lst with
	|[] -> failwith "None"
	|h::t ->  let rec helper (x,y) =
	              match y with 
	              [] -> x
	              |h'::t' -> let x' = if h' > x && f(h') then h' else x in 
	                         let y' = t'
	              in helper (x',y')
	            in helper (h,t) 


(*returns a list of all intersections connected by roads to this player. 
Has duplicates if an intersection has multiple roads branching from it*)
let rec all_intersections roadlist =
	match roadlist with
	|(e1,e2)::t -> e1::e2:: all_intersections t
	| [] -> []

(*Takes list of intersections and returns a list without duplicates*)
let rec all_ints_no_dup (roadlist: 'a list) =
	match roadlist with
	|h::t -> add_no_dup h (all_ints_no_dup t)
	| []  -> []

(*returns boolean if the intersection is connected by roads to the 
	current player's "network"*)
let rec int_is_connected gamestate i = 
	let playerints = all_ints_no_dup gamestate.curr_player.p_roads in 
	int_in_list i playerints


let rec ifromid_list gamestate (intlist: int list) : intersection list =
	List.map (i_from_id gamestate.curr_board.intersections)(intlist)
 
let rec idfromi_list (intidlist: intersection list) :int list =
	match intidlist with
	|h::t -> h.id :: idfromi_list t 
	| []  -> []


(*returns true if there is a road that matches these endpoints*)
let rec has_road roadlist intersecpair = 
	match roadlist with
	|h::t -> if h=intersecpair then true else has_road t intersecpair
	|[]	  -> false 


(*takes the intersection list and returns a list of inital intersections that can 
be settled. Must not have a settlement or an adjacent settlement *)
let rec available_int (interseclist: intersection list): intersection list=
match interseclist with 
|h::t -> (match h.settlement with |None -> h:: available_int t 
								 |Some a -> available_int t )
| []  -> []

let no_adjacent gamestate (inter:intersection) = 
	let lst = inter.adj_intersects |> ifromid_list gamestate |> available_int in 
	List.length lst = List.length inter.adj_intersects 

(*Initial settlements, can be built w/o road as long as there is no settlement 
there already or any adjacent settlements*)
let can_settle_init gamestate (intersectionlist)= 
let empty = intersectionlist |> available_int in 
let rec check intlist = match intlist with 
 |h::t -> if no_adjacent gamestate h then h::check t else check t 
 |[]   -> []
in check empty

(*Takes the intersection list and returns a list with all the settlement intersections*)
let rec settlement_ints (interseclist) = 
match interseclist with 
|h::t -> (match h.settlement with |None -> settlement_ints t 
								 |Some a -> h:: settlement_ints t )
| []  -> []

(*returns the already built roads branching from the intersection*)
let rec intersection_roads roadlist intersect =
	match roadlist with
	|(e1,e2)::t ->  if (e1.id = intersect.id || e2.id = intersect.id) then 
					(e1,e2) :: (intersection_roads t intersect) 
					else (intersection_roads t intersect) 
	| []		-> [] 

(*returns all possible roads branching from the intersection, with the 
given intersection fst and the neighbor snd*)
let pos_intersection_roads gamestate (intersec: intersection) = 
	let rec pos_roads neighbors = 
		match neighbors with 
		|h::t -> let hi = (i_from_id gamestate.curr_board.intersections h) in 
			(intersec,hi) :: pos_roads t 
		| []  -> []
	in pos_roads intersec.adj_intersects

(*Compute the desireability of an intersection to settle. Based on the dice
roll numbers of adjacent hexes as well as the scarcity of the resource on the board.*)
let compute_value (gamestate : game_state) (iint : int) : float =
let intersec = (i_from_id gamestate.curr_board.intersections iint) in 
	(*Sum the occurance of this resource over your existing houses*)
let network = gamestate.curr_player.p_roads |> all_intersections |> all_ints_no_dup 
			  |> settlement_ints in 
	(*return the roll frequency of all player hexes with this resource added together*)
	let rec hexes_val (r:resource) (hexlist: hex list) : float = 
		match hexlist with
		|h::t 	-> let i = if h.h_resource= r 
						   then (float_of_int h.h_roll_frequency) 
						   else 0. in i +. (hexes_val r t) 
		|[] 	-> 0.
in 
	let rec player_num r interseclist = 
		match interseclist with 
		|h::t-> hexes_val r h.adj_hexes +. player_num r t 
		| [] -> 0.
in
	let rec resource_avgs hexlist= 
		match hexlist with  
		|h::t 	-> let player_val = sqrt((player_num h.h_resource network) +. 1.)
				    in let avg = if h.h_resource = Desert 
				   				then 0. 
				   				else (float_of_int h.h_roll_frequency)/.player_val
				   	in avg :: resource_avgs t  
		|[] 	-> []
in (List.fold_left (fun acc x -> acc +. x) 0. (resource_avgs intersec.adj_hexes))

  
(*return the best option for a players road from an intersection that already
has a settlement. Computes the neighbor with the highest unsettled neighbor*)
let best_road_settlement gamestate (i:intersection) : intersection * intersection =
	let nlist = (ifromid_list gamestate i.adj_intersects) 
	(*return list of each neighbor and their max*)
	in let rec neighbors_avgs lst = 
	match lst with 
	|h::t-> let nneighborlist = 
				(ifromid_list gamestate h.adj_intersects)
				|> available_int |> idfromi_list 
				in let values = List.map (compute_value gamestate) nneighborlist
				in let max = get_max values in (h,max) :: neighbors_avgs t
	|[]->   [] 
	in let d = nlist |> neighbors_avgs |> get_max
	in (i,fst(d)) 
	

(*Take a game state and return the intersection pairs of possible roads 
that can be built by the player*)
let get_roadspace (gamestate: game_state) : (intersection * intersection) list=
	let allroads = gamestate.curr_board.roads in
	let playerints = gamestate.curr_player.p_roads |> all_intersections |> all_ints_no_dup in
	(*go through player's intersections and return a list of all the roads for each one*)
	let rec all_pos intersecs = match intersecs with 
		|h::t -> let i = pos_intersection_roads gamestate h in i @ all_pos t 
		| []  -> []
	in let rec all_available posintersec builtroad = 
		match posintersec with 
		|h::t -> if road_in_list h builtroad then all_available t builtroad
					else h:: all_available t builtroad
		|[]   -> []
in all_available (playerints |> all_pos) allroads	

(*returns if this intersection has a house that doesn't belong to the player*)
let foriegn_haus intersection proadlist=
	if intersection.settlement = None then false else if
	int_in_list intersection proadlist then false else true 

let get_build_roadspace (gamestate: game_state) : (intersection * intersection) list=
	let roadspace = get_roadspace gamestate in 
	let playerroads= gamestate.curr_player.p_roads in 
	let rec settlement_free roadlist proad = 
	match roadlist with 
	|(e1,e2)::t -> if not(foriegn_haus (e1) proad) && not( foriegn_haus (e2) proad) 
		then (e1,e2) :: (settlement_free t proad) else settlement_free t proad
	|[] -> [] 
in settlement_free roadspace playerroads



(*Go through the roadspace of possible roads. and select the best one*)
let choose_roadspace (gamestate:game_state) : road=
	(*remove roads that are blocked by a settlement*)
	let p_roads = gamestate.curr_player.p_roads in 
	let roads = get_build_roadspace gamestate in 
	let rec get_choose rds =
		match rds with 
			|(e1,e2)::t -> let h = if int_in_list e1 p_roads
								then ((compute_value gamestate (e2).id),(e1,e2)) else
										((compute_value gamestate e1.id),(e1,e2))
							in h :: get_choose t
			| []        -> [] 
	in roads |> get_choose |> get_max_fst |> snd 



(*Finds the best new road to build, the road that goes to the nearest high value
intersection. Must be connected to the other roads. Second in pair is the further
intersection*)
let choose_road (gamestate: game_state) : intersection * intersection = 
	(*Get list of possible roads that can be built*)
	choose_roadspace gamestate 

(*returns the max intersection in this list*)
let max_intersection gamestate intersectionlist : intersection= 
	let intlst = idfromi_list intersectionlist
in let rec intvaluelist lst= match lst with 
	|h::t -> (compute_value gamestate h,h) :: intvaluelist t 
	| []  -> [] 
in let maxint= get_max_fst (intvaluelist intlst) in snd(maxint) |>
i_from_id gamestate.curr_board.intersections   


   (*Take a game state and return a list of intersections that can be settled by 
the player. Intersection must not have a town there already, must have no 
directly adjacent tonwns, and must be connected with a road. No duplicates*)
let get_townspace (gamestate : game_state) : intersection list = 
	let empty_noneighbors endpoint=
		let empty endpoint= (match endpoint.settlement with 
			|None -> true 
			|_ -> false) in
		let noneighbors endpoint= (let conditions = List.map empty (
			ifromid_list gamestate endpoint.adj_intersects)
			in List.fold_left (&&) true conditions)
	in (empty endpoint) && (noneighbors endpoint)
	in let rec get_intersections allints = 
		match allints with
			|h::t -> if empty_noneighbors h 
					 then h :: (get_intersections t) 
					 else get_intersections t 
			|[]	-> []
	in let ints = gamestate.curr_player.p_roads |> all_intersections |>
					all_ints_no_dup
	in get_intersections ints

let rec contains movelist move = 
	match movelist with
	|h::t -> if h = move then true else contains t move
	|[]	  -> false


(*Given a list of intersections connected to player returns the best intersection 
from compute_value to settle on, intersection must be empty.*)
let choose_town gamestate : intersection = 
let intersections = get_townspace gamestate in max_intersection gamestate intersections	

(*Finds the best place to put a city via compute_value. Intersection
must have a town there already.*)
let choose_city (gamestate: game_state) : intersection = 
let ints = gamestate.curr_player.p_roads |> all_intersections |> all_ints_no_dup
in let rec has_town intlist: intersection list = 
	match intlist with
	|h::t -> (match h.settlement with 
					| Some a -> (match a.settle_type with 
											|Town -> h:: has_town t 
											|_   -> has_town t) 
					| None 	 -> has_town t) 
	| []  -> []
in ints |> has_town |> get_max


let rec number_resource (r: resource)(inv : resource list) = 
	match inv with 
	|h::t -> let i = if h = r then 1 else 0 in i + number_resource r t 
	| []  -> 0 

let four_of_one (inventory:resource list): resource option=
if number_resource Sheep inventory >= 4 then Some Sheep
	else if number_resource Ore inventory >= 4 then Some Ore 
	   	 else if number_resource Brick inventory >= 4 then Some Brick 
	   		  else if number_resource Lumber inventory >= 4 then Some Lumber
	   				else if number_resource Wheat inventory >= 4 then Some Wheat
	   					 else None
(*choose the resource you would like to get from the bank, pick the "rareest" resource to your board*)
let choose_resource gamestate : (resource) =
	let inv = gamestate.curr_player.p_inventory in 
	if in_list Lumber inv
		then if in_list Brick inv
			then if in_list Sheep inv 
				then if in_list Wheat inv
					then Ore 
				else Wheat
			else Sheep 
		else Brick 
	else Lumber


(*Takes the game state and returns a list of the possible moves that can be made, Trade 
is always an option and the moves are in no paticular order*)
let build_list (gamestate: game_state) : premove list = 
	let inventory = gamestate.curr_player.p_inventory in 
	let buildlist = 
		let blroad= 	if number_resource Brick inventory >= 1 
							&& number_resource Lumber inventory>= 1 
						then PM_Road :: [] else [] 
		in let bltown= 	if number_resource  Brick inventory >= 1 
							&& number_resource Lumber inventory>= 1 
							&& number_resource Wheat inventory >=1 
							&& number_resource Sheep inventory >= 1 
						then PM_Town :: blroad else blroad 
		in let bldev = 	if number_resource Ore inventory >= 1 
							&& number_resource Sheep inventory >= 1 
							&& number_resource Wheat inventory >= 1 
						then PM_DevCard :: bltown else bltown 
		in let blcity=	if number_resource Ore inventory >= 3 
							&& number_resource Wheat inventory>= 2 
						then PM_City :: bldev else bldev
		in				if four_of_one inventory <> None 
						then PM_Trade :: blcity else blcity
	in buildlist



(*Takes the game list and returns the best move out of the list*)
let build_choose (gamestate: game_state)(movelist: premove list) : move = 
if movelist=[] then M_EndTurn else 
let rs = get_build_roadspace gamestate in
let ts = get_townspace gamestate in 
	if ts=[] then
		if (contains movelist PM_Road && rs <> []) then M_Road (choose_road gamestate)
		else if contains movelist PM_City then M_City (choose_city gamestate)
			 else if contains movelist PM_DevCard then M_DevCard 
				  else if contains movelist PM_Trade 
				  then let x = match four_of_one (gamestate.curr_player.p_inventory) 
				  with |Some r-> r |None -> failwith "Build List Error" 
				 	in M_Trade (x, choose_resource gamestate)
					   else M_EndTurn
	else if List.length ts >= 2 then 
		if contains movelist PM_Town then M_Town (choose_town gamestate) 
			else if contains movelist PM_City then M_City (choose_city gamestate)
				else if contains movelist PM_DevCard then M_DevCard
					else if contains movelist PM_Trade
					then let x = match four_of_one (gamestate.curr_player.p_inventory) 
				 	with |Some r-> r |None -> failwith "Build List Error" 
				 	in M_Trade (x, choose_resource gamestate)
					   else M_EndTurn
	else 
		let bestnearby = choose_road gamestate in let best = choose_town gamestate in
		if snd(bestnearby) > best then 
			if (contains movelist PM_Road && rs <> []) then M_Road (bestnearby)
			else if contains movelist PM_City then M_City (choose_city gamestate)
			 else if contains movelist PM_DevCard then M_DevCard 
				  else if contains movelist PM_Trade 
				  then let x = match four_of_one (gamestate.curr_player.p_inventory) 
				 	with |Some r-> r |None -> failwith "Build List Error" 
				 	in M_Trade (x, choose_resource gamestate)
					   else M_EndTurn
		else 
			if contains movelist PM_City then M_City (choose_city gamestate)
				else if contains movelist PM_DevCard then M_DevCard
					else if contains movelist PM_Trade
					then let x = match four_of_one (gamestate.curr_player.p_inventory) 
				 	with |Some r-> r |None -> failwith "Build List Error" 
				 	in M_Trade (x, choose_resource gamestate)
					   else M_EndTurn


let year_of_plenty gamestate : resource * resource = 
	(choose_resource gamestate,choose_resource gamestate)

let monopoly gamestate : resource = choose_resource gamestate

let road_build gamestate : road =  (choose_road gamestate)  


let knight (gamestate:game_state) : p_color * hex = 
	let me = gamestate.curr_player.p_color in  
	(*returns 0 if curr_player is there, else returns number of oponents there*)
	let rec hex_people_score (intersectionlist: intersection list) =
	match intersectionlist with 
	|h::t -> let x = (match h.settlement with |Some s -> if s.owner = me 
															then (-3) else 1
								   |None   -> 0 ) in x + hex_people_score t 
	| []  -> 0
in
	let hexlist = gamestate.curr_board.hex_tiles in 
	let rec search_hexlist hxlst = 
	match hxlst with 
	|h::t -> let i = hex_people_score (ifromid_list gamestate h.h_intersections) in  
				(i,h) :: search_hexlist t 
	| []  -> []
in let options = search_hexlist hexlist in let hex = (get_max_fst options) |> snd
in let rec find_player intersections : p_color = 
match intersections with 
|h::t -> (match h.settlement with |Some s-> s.owner |None -> find_player t)
| []  -> failwith "No opponents"
in let player = find_player (ifromid_list gamestate hex.h_intersections) 
in (player, hex)



(*Takes the initial turn for the AI, returns location of one house and one road*)
let init_move (gamestate: game_state) : move * move= 
	let intersections = gamestate.curr_board.intersections 
			|> can_settle_init gamestate in 
	let house = max_intersection gamestate intersections
	in let road = best_road_settlement gamestate house
	in (M_Town house , M_Road road)


let main (gamestate: game_state) : move = 
	gamestate |> build_list |> build_choose gamestate



