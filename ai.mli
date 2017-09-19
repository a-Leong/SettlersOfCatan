open Game


type premove = 
			| PM_DevCard 
			| PM_Road 
			| PM_Town 
			| PM_City 
			| PM_Trade 
			| PM_Port 
			| PM_EndTurn


(*Take a game state and return a list of intersections that can be settled.
Intersection mustnot have a town there already, must have no directly 
adjacent towns, and must be connected with a road. *)
val get_townspace : game_state -> intersection list

(*Compute the desireability of an intersection to settle. Based on the dice
roll numbers as well as the scarcity of the resource on the board.*)
val compute_value : game_state -> int ->  float

(*returns a list of all intersections connected by roads to this player. 
Has duplicates if an intersection has multiple roads branching from it*)
val all_intersections :  road list -> intersection list


(*Takes the game list and returns the best move out of the list*)
val build_choose : game_state -> premove list -> move

(*Given a list of intersections returns the best intersection from compute_value.
List is not empty. *)
val choose_town : game_state -> intersection

(*Finds the best place to put a city via compute_value. Intersection
must have a town there already.*)
val choose_city : game_state -> intersection

(*Finds the best new road to build, the road that goes to the nearest high value
intersection. Must be connected to the other roads.*)
val choose_road : game_state -> intersection * intersection

(*Takes the initial turn for the AI, places one house and one road, with the
road connected to the house*)
val init_move : game_state-> (move * move)



val year_of_plenty : game_state -> (resource * resource) 

val monopoly: game_state -> resource

val road_build: game_state -> road   

val knight: game_state -> p_color * hex

(*Returns the gamestate after the AI has made all their moves*)
val main : game_state -> move

