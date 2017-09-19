(* Code written by Alex Leong *)

open Array
open Random

exception Cannot_build of string
exception Cannot_trade of string
exception Cannot_play of string

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

let rec amount lst acc r =
    match lst with
    | []    ->  acc
    | h::t  ->  if h == r then amount t (acc + 1) r else amount t acc r

let string_of_r (r : resource) : string =
    match r with
    | Brick -> "brick" | Lumber -> "lumber" | Ore       -> "ore" 
    | Wheat -> "wheat" | Sheep  -> "sheep"  | Desert    -> "desert"

let string_of_rl (rl : resource list) : string =
    let rs = [Brick;Lumber;Ore;Sheep;Wheat] in 
    let nums = List.map (amount rl 0) rs in
    List.fold_left2 
    (fun x y z  ->  if y == 0 then 
                        x
                    else if String.equal x "" then
                        (string_of_int y) ^ " " ^ (string_of_r z)
                    else 
                        x ^ ", " ^ (string_of_int y) ^ " " ^ (string_of_r z))
    "" nums rs

let string_of_pc (p : p_color) : string = 
    match p with
    | Red -> "Red" | Blue -> "Blue" | Green -> "Green" | Yellow -> "Yellow"

let string_of_d (d : devcard) : string = 
    match d with
    | Knight        ->  "Knight"    | Victory_pt        ->  "Victory Point"
    | Monopoly      ->  "Monopoly"  | Year_of_plenty    ->  "Year of Plenty"
    | Road_building ->  "Road Building"

let string_of_dl (dl : devcard list) : string = 
    let ds = [Knight;Monopoly;Road_building;Victory_pt;Year_of_plenty] in 
    let nums = List.map (amount dl 0) ds in
    List.fold_left2 
    (fun x y z  ->  if y == 0 then 
                        x
                    else if String.equal x "" then
                        (string_of_int y) ^ " " ^ (string_of_d z)
                    else 
                        x ^ ", " ^ (string_of_int y) ^ " " ^ (string_of_d z))
    "" nums ds

let dc_description (d : devcard) : string = 
    match d with
    | Knight        ->  "Move the robber. Steal 1 resource from the owner of a " ^ 
                        "settlement or city adjacent to the robber’s new hex."    
    | Victory_pt    ->  "Reveal this card on your turn if, with it, you reach " ^
                        "the number of points required for victory."
    | Monopoly      ->  "When you play this card, announce 1 type of resource. " ^
                        "All other players must give you all of their " ^
                        "resources of that type."  
    | Year_of_plenty->  "If you play this card you may immediately take any 2 " ^
                        "resource cards from the supply stacks. You may use " ^
                        "these cards to build in the same turn."
    | Road_building ->  "If you play this card, you may immediately place 2 " ^
                        "free roads on the board (according to normal " ^
                        "building rules)."

(** I wrote this function after looking at the following page on Stack Overflow:
 *  http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml
 *  It is a O(n) list shuffling function which will be used to further randomize
 *  the lists generated that initialize a randomized game_state.
 *
 *  [shuffle lst] is a list that contains exactly every element in lst, in 
 *  a random order. *)
let shuffle (lst : 'a list) : 'a list =
    let assigned = List.map (fun c -> (Random.bits (), c)) lst in
    let new_order = List.sort compare assigned in
    List.map snd new_order

(** [get_frequency roll] is an int 0...5 which cooresponds to the frequncy
 *  of roll occuring as the sum of two random ints between 1...12. *)
let get_frequency (roll : int) : int =
    match roll with
    | 2     -> 1
    | 3     -> 2
    | 4     -> 3
    | 5     -> 4
    | 6     -> 5
    | 8     -> 5
    | 9     -> 4
    | 10    -> 3
    | 11    -> 2
    | 12    -> 1
    | _     -> 0

(** [i_from_id il id] is the intersection with id == [id]. *)
let rec i_from_id (il : intersection list) (id : int) : intersection =
    match il with
    | []    ->  let id_string = string_of_int id in
                raise (Failure ("can't find intersection ID: " ^ id_string))
    | h::t  ->  if h.id == id then h
                else i_from_id t id

(** [h_from_id hl id] is the hex with id == [id]. *)
let rec h_from_id (hl : hex list) (id : int) : hex =
    match hl with
    | []    ->  raise (Failure ("can't find hex ID: "^(string_of_int id)))
    | h::t  ->  if h.h_id == id then h
                else h_from_id t id

(** [robber_start_hex hl] is the hex which has resource Desert and roll number
 *   0. Raises Not_found if [hl] does not contain at least one Desert tile. *)
let rec robber_start_hex (hl : hex list) : hex =
    match hl with
    | []    ->  raise (Failure "No Desert hex found")
    | h::t  ->  if h.h_resource == Desert then h
                else robber_start_hex t

(** [[get_adj_intersects i il] is the int list containing the ids
 *  of each intersection adjacent to [i]. *)
let get_adj_intersects (i : intersection) (il : intersection list) : int list =
    let id = i.id in
    let adjs = ref [] in
    let f () = if id == 30 then 
                    (adjs := 1::!adjs; adjs := 29::!adjs)
                else if id == 1 then
                    (adjs := 2::!adjs; adjs := 30::!adjs)
                else 
                    () in
    let g () = if id == 48 then 
                    (adjs := 31::!adjs; adjs := 47::!adjs)
                else if id == 31 then
                    (adjs := 48::!adjs; adjs := 32::!adjs)
                else
                    () in
    let h () = if id == 54 then 
                    (adjs := 53::!adjs; adjs := 49::!adjs)
                else if id == 49 then
                    (adjs := 54::!adjs; adjs := 50::!adjs)
                else
                    () in       
    let () = 
    (
        if id < 31 then
            (if (id - 1) mod 5 == 0 || (id + 1) mod 5 == 0 then
                (if id == 1 then adjs := 31::!adjs
                else if id < 7 then adjs := (id + 28)::!adjs
                else if id < 12 then adjs := (id + 26)::!adjs
                else if id < 17 then adjs := (id + 24)::!adjs
                else if id < 22 then adjs := (id + 22)::!adjs
                else if id < 27 then adjs := (id + 20)::!adjs
                else if id == 29 then adjs := 47::!adjs)
            else ())

        else if id < 49 then
            (if id mod 3 == 0 then
                (if id == 33 then adjs := 49::!adjs
                    else if id == 36 then adjs := 50::!adjs
                    else if id == 39 then adjs := 51::!adjs
                    else if id == 42 then adjs := 52::!adjs
                    else if id == 45 then adjs := 53::!adjs
                    else if id == 48 then adjs := 54::!adjs)
            else ())
        else
            ()
    ) in 
    let corner_cases = [1;30;31;48;49;54] in
    let () = if List.mem id corner_cases 
        then ()
    else
        (adjs := (id + 1)::!adjs; adjs := (id - 1)::!adjs) in
    let () = f(); g(); h() in
    !adjs

(** [make_intersections ()] is a list of 55 intersections whose
 *  fields are initialized empty except for their ids which are
 *  1...54. *)
let rec make_intersections () : intersection list = 
    let rec loop acc i =
        match i with
        | 55 -> acc
        | a  -> let new_intersection = 
                    {
                        id = a;
                        settlement = None;
                        port = None;
                        adj_hexes = [];
                        adj_intersects = [];
                    } in
                loop (new_intersection::acc) (a + 1)
    in
    loop [] 1

(** [add_adjacent_intersections il] is the intersection list [il] whose
 *  elemnts' adj_intersects fields are correctly filled. *)
let add_adjacent_intersections (il : intersection list) : intersection list =
    let rec fill ia (il : intersection list) = 
        match il with
        | []    ->  ()
        | h::t  ->  let rec x (i : intersection) ia (il : int list) : unit = 
                    match il with
                    | []    ->  ()
                    | h::t  ->  let o = i_from_id ia h in
                                let dup = List.mem i.id o.adj_intersects in
                                if List.mem h i.adj_intersects && not dup then (
                                    o.adj_intersects <- i.id::o.adj_intersects;
                                    x i ia t)
                                else
                                    x i ia t
                    in 
                    let idl = h.adj_intersects in
                    let () = x h ia idl in
                    fill ia t
    in
    let rec loop (acc : intersection list) ia (il : intersection list) =
        match il with
        | []    ->  acc
        | h::t  ->  let intersects = get_adj_intersects h ia in
                    let new_i = {h with adj_intersects = intersects} in
                    loop (new_i::acc) ia t
        in
        let is = loop [] il il in
        let () = fill is (List.rev is) in
        is

(** [add_adjacent_hexes il hl] is the intersection list il where each
 *   intersection's adj_hexes *)
let add_adjacent_hexes (il : intersection list) (hl : hex list)
                        : intersection list =
    let rec fill hl il =
        match hl with
        | []     -> ()
        | h1::t1 -> let rec x intl hex ia =
                        match intl with
                        | []     -> fill t1 il
                        | h2::t2 -> let inters = i_from_id ia h2 in
                                    inters.adj_hexes <- hex::inters.adj_hexes;
                                    x t2 hex ia
                    in x h1.h_intersections h1 il
    in
    let () = fill hl il in
    il

(** [gen_resources ()] is a randomly ordered, length 18 list of resources
 *  where no single resource occurs in the list more than 4 times, and
 *  Desert occurs at most 1 time. *)
let gen_resources () : resource list =
    let b = ref 0 in
    let l = ref 0 in
    let o = ref 0 in
    let w = ref 0 in
    let s = ref 0 in
    let () = Random.self_init () in
    let rec loop acc =
        match List.length acc with
        | 18 
                ->  (Desert::acc)
        | _
                ->  let rand = Random.int 5 in
                    if rand == 0 && !b < 4 then
                        let () = b := !b + 1 in
                        loop (Brick::acc)
                    else if rand == 1 && !l < 4 then
                        let () = l := !l + 1 in
                        loop (Lumber::acc)
                    else if rand == 2 && !o < 4 then
                        let () = o := !o + 1 in
                        loop (Ore::acc)
                    else if rand == 3 && !w < 4 then
                        let () = w := !w + 1 in
                        loop (Wheat::acc)
                    else if rand == 4 && !s < 4 then
                        let () = s := !s + 1 in
                        loop (Sheep::acc)
                    else loop acc
    in shuffle (loop [])

(** [gen_roll_numbers ()] is a randomly ordered, length 19 list of ints
 *  {2...12}/{7} where no int occurs in the list more than 3 times,
 *  and 2 and 12 occur at most 1 time. *)
let gen_roll_numbers () : int list =
    let two = ref 0 in
    let three = ref 0 in
    let four = ref 0 in
    let five = ref 0 in
    let six = ref 0 in
    let eight = ref 0 in
    let nine = ref 0 in
    let ten = ref 0 in
    let eleven = ref 0 in
    let twelve = ref 0 in
    let () = Random.self_init () in
    let rec loop acc =
        match List.length acc with
        | 18    
                ->  acc
        | _ 
                ->  let rand = Random.int 13 in
                    if rand == 2 && !two < 1 then
                        let () = two := !two + 1 in
                        loop (2::acc)
                    else if rand == 3 && !three < 2 then
                        let () = three := !three + 1 in
                        loop (3::acc)
                    else if rand == 4 && !four < 2 then
                        let () = four := !four + 1 in
                        loop (4::acc)
                    else if rand == 5 && !five < 2 then
                        let () = five := !five + 1 in
                        loop (5::acc)
                    else if rand == 6 && !six < 2  then
                        let () = six := !six + 1 in
                        loop (6::acc)
                    else if rand == 8 && !eight < 2 then
                        let () = eight := !eight + 1 in
                        loop (8::acc)
                    else if rand == 9 && !nine < 2 then
                        let () = nine := !nine + 1 in
                        loop (9::acc)
                    else if rand == 10 && !ten < 2 then
                        let () = ten := !ten + 1 in
                        loop (10::acc)
                    else if rand == 11 && !eleven < 2 then
                        let () = eleven := !eleven + 1 in
                        loop (11::acc)
                    else if rand == 12 && !twelve < 1 then
                        let () = twelve := !twelve + 1 in
                        loop (12::acc)
                    else loop acc
    in shuffle (loop [])

(** [gen_devcards ()] is a length 25 list of devcards with distribution:
 *  14 Knight, 5 Victory_pt, 2 Road_building, 2 Monopoly, 2 Year_of_plenty. *)
let gen_devcards () : devcard list =
    let knight = ref 0 in
    let victorypt = ref 0 in
    let road_building = ref 0 in
    let monopoly = ref 0 in
    let year_of_plenty = ref 0 in
    let () = Random.self_init () in
    let rec loop acc =
        match List.length acc with
        | 25 
                ->  acc
        | _
                ->  let rand = Random.int 5 in
                    match rand with
                    | 0 when !knight < 14 
                            ->  knight := !knight + 1;
                                loop (Knight::acc)
                    | 1 when !victorypt < 5
                            ->  victorypt := !victorypt + 1;
                                loop (Victory_pt::acc)
                    | 2 when !road_building < 2
                            ->  road_building := !road_building + 1;
                                loop (Road_building::acc) 
                    | 3 when !monopoly < 2
                            ->  monopoly := !monopoly + 1;
                                loop (Monopoly::acc)
                    | 4 when !year_of_plenty < 2
                            ->  year_of_plenty := !year_of_plenty + 1;
                                loop (Year_of_plenty::acc) 
                    | _
                            ->  loop acc
    in shuffle (loop [])


(** [make_hexes ()] is a list of 19 hexes whose
 *  fields are initialized to appropriate values; ids are 1...19. *)
let hexes_init () : hex list = 
    let numbers = gen_roll_numbers () in
    let resources = gen_resources () in
    let rec loop numbers resources acc i =
        if List.length numbers > 0 && List.length resources > 0 then
            let n = List.hd numbers in
            let r = List.hd resources in
            let intersections = 
                if i == 1 then [1;2;3;4;31;32]
                else if i == 2 then [4;5;6;32;33;34]
                else if i == 3 then [6;7;8;9;34;35]
                else if i == 4 then [9;10;11;35;36;37]
                else if i == 5 then [11;12;13;14;37;38]
                else if i == 6 then [14;15;16;38;39;40]
                else if i == 7 then [16;17;18;19;41;40]
                else if i == 8 then [19;20;21;41;42;43]
                else if i == 9 then [21;22;23;24;43;43]
                else if i == 10 then [24;25;26;44;45;46]
                else if i == 11 then [26;27;28;29;46;47]
                else if i == 12 then [29;30;1;31;47;48]
                else if i == 13 then [31;32;33;48;54;49]
                else if i == 14 then [33;34;35;36;49;50]
                else if i == 15 then [36;37;38;39;50;51]
                else if i == 16 then [39;40;41;42;51;52]
                else if i == 17 then [42;43;44;45;52;53]
                else if i == 18 then [45;46;47;48;53;54]
                else [49;50;51;52;53;54] in
            let rob = if r == Desert then true else false in
            match i with 
            | a  -> let new_hex = 
                        {
                            h_id = a;
                            h_resource = r;
                            h_robber_pres = rob;
                            h_intersections = intersections;
                            h_roll_number = 
                                if rob then 0 else n;
                            h_roll_frequency = 
                                if rob then 0 else get_frequency n;
                        } in
                    let nums = if rob then numbers else List.tl numbers in 
                    loop nums (List.tl resources) (new_hex::acc) (a + 1)
        else
            acc
    in
    loop numbers resources [] 1

(** [valid_board hl] is true when no two adjacent hexes have roll frequencies
 *  of 5 and false otherwise. *)
let valid_board (hl : hex list) : bool = 
    let adj_hexes (h : int) : int list =
        match h with
        | 1  -> [2;12;13]
        | 2  -> [1;13;14;3]
        | 3  -> [2;14;4]
        | 4  -> [3;5;14;15]
        | 5  -> [4;6;15]
        | 6  -> [5;7;15;16]
        | 7  -> [6;8;16]
        | 8  -> [7;9;16;17]
        | 9  -> [8;10;17]
        | 10 -> [9;11;17;18]
        | 11 -> [10;12;18]
        | 12 -> [1;11;13;18]
        | 13 -> [1;2;12;18;19;14]
        | 14 -> [2;3;4;13;19;15]
        | 15 -> [4;5;6;14;16;19]
        | 16 -> [6;7;8;15;17;19]
        | 17 -> [8;9;10;16;18;19]
        | 18 -> [10;11;12;13;17;19]
        | 19 -> [13;14;15;16;17;18]
        | _  -> []
    in
    let rec loop hl ha =
        match hl with
        | []    ->  true
        | h::t  ->  let adjs = adj_hexes h.h_id in
                    let rec check_adjs adjs ha =
                        match adjs with
                        | []    ->  true
                        | h::t  ->  let hex = h_from_id ha h in 
                                    if hex.h_roll_frequency == 5 then false
                                    else true && check_adjs t ha
                    in 
                    if h.h_roll_frequency == 5 then
                        loop t ha && (check_adjs adjs ha)
                    else loop t ha
    in loop hl hl



(** [board_init ()] is a valid board whose fields are initialized. *)
let board_init () : board =
    let intersects = make_intersections () in
    let hexes = ref (hexes_init ()) in
    let () =    while not (valid_board !hexes) do
                    hexes := hexes_init ()
                done in 
    let intersects = 
        add_adjacent_hexes (add_adjacent_intersections intersects) !hexes in
    {
        hex_tiles = !hexes;
        intersections = intersects;
        robber_location = robber_start_hex !hexes;
        roads = [];
    }

(** [players_init ()] is a length 4 list of players whose fields are 
 *  initialized. Red player is the human, Blue Green and Yellow are AI. *)
let players_init () : player list =
    let rec loop acc =
        let len = List.length acc in
        match len with
        | 4 -> acc
        | _ ->  let color_ai = match len with
                            | 0 -> (Red,false)
                            | 1 -> (Blue,true)
                            | 2 -> (Green,true)
                            | _ -> (Yellow,true)
                in
                let new_player =    {
                                        p_color = fst color_ai;
                                        p_ai = snd color_ai;
                                        p_inventory = [];
                                        p_victorypts = 0;
                                        p_devcards = [];
                                        p_knights = 0;
                                        p_ports = [];
                                        p_settled = [];
                                        p_start_road1 = None;
                                        p_start_road2 = None;
                                        p_roads = [];
                                        p_road_supply = 15;
                                        p_town_supply = 5;
                                        p_city_supply = 4;
                                    } in
                loop (new_player::acc)
    in List.rev (loop [])


(** [game_init ()] is the game_state at the beginning of a game. Board is
 *  randomly generated according to Settlers rules, four players are 
 *  initialized, and the first player is randomly chosen from the four. *)
let rec game_init () : game_state =
    try
        let board = board_init () in
        let players = players_init () in
        let devcards = gen_devcards () in
        let first_player = List.nth players (Random.int 4) in
        {
            curr_board = board;
            curr_player = first_player;
            players = players;
            devcard_pile = devcards;
            victory_pt_limit = 10;
            longest_road = None;
            largest_army = None;
        } 
    with | _ -> game_init ()
(** [p_from_color color g] is the player with p_color == [color]. *)
let rec p_from_color (color : p_color) (pl : player list) : player =
    match pl with
    | []    ->  raise Not_found
    | h::t  ->  if h.p_color == color then h
                else p_from_color color t

(** [has_resource player r num] is true if [player] has at least [num]
 *  resources of type [r] in their inventory and is false otherwise. *)
let has_resource (player : player) (r : resource) (num : int) : bool =
    let i = ref 0 in
    let inv = player.p_inventory in
    let rec loop rl r num =
        match rl with
        | []    ->  false
        | h::t  ->  let () =    if h == r then i := !i + 1
                                else () in
                    if !i == num then true
                    else loop t r num
    in loop inv r num 

(** [remove_resource player resource] removes one resource of type
 *  [r] from the inventory of [player], raises exception Not_found
 *  if [player] does not have any [r] in their inventory. *)
let remove_resource (player : player) (r : resource) : unit =
    let rec loop rl acc r =
        match rl with
        | [] -> raise Not_found
        | h::t ->   if h == r then t @ acc
                    else loop t (h::acc) r
    in
    player.p_inventory <- loop player.p_inventory [] r

(** [remove_devcard player dc] removes one devcard of type
 *  [dc] from the inventory of [player], raises exception Not_found
 *  if [player] does not have any [dc] in their inventory. *)
let remove_devcard (player : player) (dc : devcard) : unit =
    let rec loop rl acc r =
        match rl with
        | [] -> raise Not_found
        | h::t ->   if h == dc then t @ acc
                    else loop t (h::acc) dc
    in
    player.p_devcards <- loop player.p_devcards [] dc

(* [roll_dice ()] is an int in [2...12] with number frequency following the
 *  normal distribution with mean 7. *)
let roll_dice () : int =
    let () = Random.self_init () in
    let d1 = (Random.int 6) + 1 in
    let d2 = (Random.int 6) + 1 in
    d1 + d2

(** [adjacent_to_road i1 i2 g] is true if either [i1] or [i2] are adjacent
 *  to a road owned by the current player and is false otherwise. *)
let adjacent_to_road (i1 : intersection) (i2 : intersection) (g : game_state)
                     : bool = 
let roads = g.curr_player.p_roads in
let rec traverse id1 id2 roads =
    match roads with
    | (id1',id2')::t -> if(id1 = id1'.id || id1 = id2'.id || id2 = id1'.id || id2 = id1'.id) then
                        true
                        else traverse id1 id2 t
    | [] -> false
in
    traverse (i1.id) (i2.id) roads

(** [adjacent_to_settlement i1 i2 g] is true when either [i1] or [i2] is 
 *  adjacent to a settlement owned by the current player. *)
let adjacent_to_settlement  (i1 : intersection) (i2 : intersection) 
                            (g : game_state) : bool =
    let settlements = g.curr_player.p_settled in
    List.mem i1.id settlements || List.mem i2.id settlements

(** Checks to see if the current player has reached or exceeded the victory
 *  point limit. If they have, prints a victory message, else returns unit. *)
let check_win (g : game_state) : unit =
    let p = g.curr_player in
    if p.p_victorypts >= g.victory_pt_limit then
        let victory = (string_of_pc p.p_color) ^ " wins the game!" ^
                        "\n Thanks for playing!" in
        ANSITerminal.(print_string [red] victory);
        exit 0
    else
        ()

(** Runs after each instance of knight being played or a road being built*)
let update_trophy (g : game_state) : unit =
    let longest_road g : player option =
        let p_roads = List.map (fun c -> (List.length c.p_roads, c)) g.players in
        let most_first = List.rev (List.sort compare p_roads) in
        let players = List.map snd most_first in
        let most_roads =    match g.longest_road with
                            | Some p    ->  List.length p.p_roads
                            | None      ->  4
        in
        match List.hd players with
        | p when List.length p.p_roads > most_roads 
                ->  let () =    print_endline ((string_of_pc p.p_color) ^ 
                                " now has the longest road!");
                                match g.longest_road with
                                | Some prev ->  p.p_victorypts <- p.p_victorypts + 2;
                                                prev.p_victorypts <- prev.p_victorypts - 2
                                | None -> p.p_victorypts <- p.p_victorypts + 2; in
                    Some p
        | _
                ->  g.longest_road
    in
    let largest_army g : player option =
        let p_knights = List.map (fun c -> (c.p_knights, c)) g.players in
        let most_first = List.rev (List.sort compare p_knights) in
        let players = List.map snd most_first in
        let most_knights =  match g.largest_army with
                            | Some p    ->  p.p_knights
                            | None      ->  2
        in
        match List.hd players with
        | p when p.p_knights > most_knights 
                ->  let () =    print_endline ((string_of_pc p.p_color) ^ 
                                " now has the largest army!");
                                match g.largest_army with
                                | Some prev ->  p.p_victorypts <- p.p_victorypts + 2;
                                                prev.p_victorypts <- prev.p_victorypts - 2
                                | None -> p.p_victorypts <- p.p_victorypts + 2; in
                    Some p
        | _
                ->  g.largest_army
    in
    g.longest_road <- longest_road g;
    g.largest_army <- largest_army g;
    check_win g

(*Take a game state and return a list of intersections that can be settled by 
the player. Intersection must not have a town there already, must have no 
directly adjacent tonwns, and must be connected with a road. 

+++++ this function copied from Ai.ml credit to Beau ++++++

*)
let get_townspace (g : game_state) (setup : bool): intersection list = 
    let rec ifromid_list intersectionlist idlist =
        match idlist with
        | h::t  -> i_from_id intersectionlist h :: ifromid_list intersectionlist t
        | []    -> [] 
    in

    let rec all_intersections roadlist : intersection list=
        match roadlist with
        | (e1,e2)::t    -> e1::e2:: all_intersections t
        | []            -> [] 
    in 

    let empty_noneighbors endpoint=
        let empty endpoint = match endpoint.settlement with 
            | None  -> true 
            | _     -> false
        in

        let noneighbors endpoint= 
            let conditions = List.map empty 
                (ifromid_list g.curr_board.intersections endpoint.adj_intersects)
            in List.fold_left (&&) true conditions
        in (empty endpoint) && (noneighbors endpoint) 
    in 

    let all_ints =  if setup then g.curr_board.intersections
                    else all_intersections g.curr_player.p_roads in 

    let rec get_intersections allints = 
        match allints with
        | h::t  ->  if empty_noneighbors h then 
                        h::(get_intersections t) 
                    else get_intersections t 
        | []    -> []
    in 

    get_intersections all_ints

(** Takes two intersection IDs, places a town/city at specified intersection
 *  if legal, else raises exception Cannot_build  *)
let setup_road (i1 : int) (i2 : int) (g : game_state) : unit =
    let p = g.curr_player in
    let last_settled = List.hd p.p_settled in
    let i1 = i_from_id g.curr_board.intersections i1 in
    let i2 = i_from_id g.curr_board.intersections i2 in
    let can_build = (i1.id == last_settled || i2.id == last_settled) &&
                    (List.mem i2.id i1.adj_intersects)
    in
    if can_build then (
        g.curr_board.roads <- (i1,i2)::g.curr_board.roads;
        p.p_roads <- (i1,i2)::p.p_roads;
        p.p_road_supply <- p.p_road_supply - 1;
        (match p.p_start_road1 with
         | Some r   ->  p.p_start_road2 <- Some (i1,i2)
         | None     ->  p.p_start_road1 <- Some (i1,i2));
        print_endline "Road placed")
    else
        raise (Cannot_build ("Can't place road at"^(string_of_int i1.id)^","^(string_of_int i2.id)))

let road_possible (g : game_state) : bool =
    let p = g.curr_player in
    has_resource p Brick 1  &&
    has_resource p Lumber 1 &&
    p.p_road_supply > 0

(** Takes in two intersection IDs and a game state, returns an updated game
 *  state if that move is legal, else, raises exception Cannot_build *)
let build_road (i1 : int) (i2 : int) (g : game_state) : unit =
    let i1 = i_from_id g.curr_board.intersections i1 in
    let i2 = i_from_id g.curr_board.intersections i2 in
    let p = g.curr_player in
    let adj_to_road = adjacent_to_road i1 i2 g in
    let adj_to_settle = adjacent_to_settlement i1 i2 g in
    let can_build = has_resource p Brick 1  &&
                    has_resource p Lumber 1 &&
                    (* i1 and i2 are adjacent *)
                    List.mem i2.id i1.adj_intersects &&
                    (* a road from i1 to i2 does not already exist *)
                    not (List.mem (i1,i2) g.curr_board.roads ||
                         List.mem (i2,i1) g.curr_board.roads) &&
                    (* potential road is connected to player's network *)
                    (adj_to_road || adj_to_settle) &&
                    (* player has at least one road to place *)
                    p.p_road_supply > 0

    in
    if can_build then (
        g.curr_board.roads <- (i1,i2)::g.curr_board.roads;
        p.p_roads <- (i1,i2)::p.p_roads;
        p.p_road_supply <- p.p_road_supply - 1;
        remove_resource p Brick;
        remove_resource p Lumber;
        print_endline "Road built";
        update_trophy g)
    else
        raise (Cannot_build "Can't build road")

(** Takes an intersection ID, places a town/city at specified intersection 
 *  if legal, else raises exception Cannot_build  *)
let setup_town (i : int) (g : game_state) : unit = 
    let i = i_from_id g.curr_board.intersections i in
    let p = g.curr_player in
    let townspace = get_townspace g true in
    let can_build = List.mem i townspace in
    if can_build then
        let new_settlement =    
            {
                settle_type = Town;
                owner = p.p_color;
                location = i;
            } in
        p.p_settled <- i.id::p.p_settled;
        p.p_victorypts <- p.p_victorypts + 1;
        i.settlement <- Some new_settlement;
        p.p_town_supply <- p.p_town_supply - 1;
        print_endline "Town placed"
    else
        raise (Cannot_build ("Can't place town on intersection "^(string_of_int i.id)))

let town_possible (g : game_state) : bool =
    let p = g.curr_player in
    let townspace = get_townspace g false in
    List.length townspace > 0   &&
    has_resource p Brick 1      &&
    has_resource p Lumber 1     &&
    has_resource p Sheep 1      &&
    has_resource p Wheat 1      &&
    p.p_town_supply > 0

(** Takes an intersection, places a town at specified intersection if legal,
 *  else raises exception Cannot_build  *)
let build_town (i : int) (g : game_state) : unit =
    let i = i_from_id g.curr_board.intersections i in
    let p = g.curr_player in
    let townspace = get_townspace g false in
    let can_build = List.mem i townspace    &&
                    has_resource p Brick 1  &&
                    has_resource p Lumber 1 &&
                    has_resource p Sheep 1  &&
                    has_resource p Wheat 1  &&
                    p.p_town_supply > 0
    in
    if can_build then 
        let new_settlement =    
            {
                settle_type = Town;
                owner = p.p_color;
                location = i;
            } in
        p.p_settled <- i.id::p.p_settled;
        i.settlement <- Some new_settlement;
        p.p_town_supply <- p.p_town_supply - 1;
        p.p_victorypts <- p.p_victorypts + 1;
        remove_resource p Brick;
        remove_resource p Lumber;
        remove_resource p Sheep;
        remove_resource p Wheat;
        print_endline "Town built";
        check_win g
    else
        raise (Cannot_build "Can't build town")

let city_possible (g : game_state) : bool = 
    let p = g.curr_player in
    let has_town = 
        List.map (fun i ->  let i = i_from_id g.curr_board.intersections i in
                            let i = match i.settlement with
                                    | Some t -> t 
                                    | _ -> raise Not_found in
                            i.settle_type == Town) p.p_settled in
    let has_town = List.fold_left (||) false has_town in
    has_town                &&
    has_resource p Ore 3    &&
    has_resource p Wheat 2  &&
    p.p_city_supply > 0

(** Takes an intersection, places a city at specified intersection if legal,
 *  else raises exception Cannot_build  *)
let build_city (i : int) (g : game_state) : unit =
    let i = i_from_id g.curr_board.intersections i in
    let p = g.curr_player in 
    match i.settlement with
    | Some t when   t.owner == p.p_color    &&
                    t.settle_type == Town   &&
                    t.location.id == i.id   &&
                    has_resource p Ore 3    &&
                    has_resource p Wheat 2  &&
                    p.p_city_supply > 0
            ->  t.settle_type <- City;
                p.p_city_supply <- p.p_city_supply - 1;
                p.p_town_supply <- p.p_town_supply + 1;
                p.p_victorypts <- p.p_victorypts + 1;
                remove_resource p Ore;
                remove_resource p Ore;
                remove_resource p Ore;
                remove_resource p Wheat;
                remove_resource p Wheat;
                print_endline "City built";
                check_win g
    | _ 
            -> raise (Cannot_build "Can't build city")

let play_dc_possible (g : game_state) : bool =
    let p = g.curr_player in
    List.length p.p_devcards > 0

let build_dc_possible (g : game_state) : bool =
    let card_to_buy = List.length g.devcard_pile > 0 in
    let p = g.curr_player in
    card_to_buy              &&
    has_resource p Ore 1     &&
    has_resource p Wheat 1   &&
    has_resource p Sheep 1

(** Builds a development card, places it in player's devcard inventory. If
 *  insufficient resources, raises Cannot_build *)
let build_devcard (g : game_state) : unit =
    let p = g.curr_player in
    let can_build = build_dc_possible g in
    if can_build then
        let built_card = List.hd g.devcard_pile in
        remove_resource p Ore;
        remove_resource p Wheat;
        remove_resource p Sheep;
        if built_card == Victory_pt then (
            p.p_victorypts <- p.p_victorypts + 1;
            g.devcard_pile <- List.tl g.devcard_pile;
            print_endline "Victory point recieved";
            check_win g)
        else (
            p.p_devcards <- built_card::p.p_devcards;
            g.devcard_pile <- List.tl g.devcard_pile;
            print_endline "Development card purchased")
    else
        raise (Cannot_build "Can't purchase development card")

let trade_possible (g : game_state) : bool =
    let all_r = [Brick;Lumber;Ore;Sheep;Wheat] in
    let p = g.curr_player in
    let bs = List.map (fun x -> has_resource p x 4) all_r in
    List.fold_left (||) false bs

(** [bank_trade r1 r2 g] removes four of [r1] from the current player's 
 *  inventory for one of [r2]. Returns unit, updates game_state to reflect 
 *  trade if possible raises exception Cannot_trade if impossible. *)
let bank_trade (give : resource) (recieve : resource) (g : game_state) : unit =
    let p = g.curr_player in
    let can_trade = has_resource p give 4 in
    if can_trade then (
        remove_resource p give;
        remove_resource p give;
        remove_resource p give;
        remove_resource p give;
        p.p_inventory <- recieve::p.p_inventory;
        print_endline "Trade successful")
    else
        raise (Cannot_trade "Insufficient resources")

(** [add_resource r p] adds one resource [r] to the inventory of player [p]. *)
let add_resource  (p : player) (r : resource) : unit =
    p.p_inventory <- r::p.p_inventory

(** [collect_r n g] updates [g] where each player recieves the
 *  appropriate number of each resource according to their settlements
 *  (1 for each settlment and 2 for each city adjacent to rolled hexes)
 *  and the result of roll_dice [n]. *)
let collect_r (n : int) (g : game_state) : unit =
    let rec hs_from_roll (hl : hex list) (n : int) : hex list =
        match hl with
        | []    ->  []
        | h::t  ->  if h.h_roll_number == n then h::(hs_from_roll t n)
                    else hs_from_roll t n
    in

    let rec recievers il acc g : player list =
        match il with
        | []    ->  acc
        | h::t  ->  match h.settlement with
                    | (Some s)  ->  let p = p_from_color s.owner g.players in
                                    if s.settle_type == Town then   
                                        recievers t (p::acc) g
                                    else 
                                        recievers t (p::(p::acc)) g
                    | _         ->  recievers t acc g
    in

    let all_hexes = g.curr_board.hex_tiles in
    let rolled_h = hs_from_roll all_hexes n in
    let rec iterate_rolled_hexes hs g =
        match hs with
        | []    -> ()
        | h::t  ->  let ids = g.curr_board.intersections in
                    let is = List.map (i_from_id ids) h.h_intersections in
                    let pl = recievers is [] g in
                    let rec iterate_settlements pl r =
                        match pl with
                        | []    ->  ()
                        | h::t  ->  h.p_inventory <- r::h.p_inventory;
                                    iterate_settlements t r
                    in 
                    if h.h_robber_pres then
                        iterate_rolled_hexes t g
                    else
                        iterate_settlements pl h.h_resource;
                        iterate_rolled_hexes t g
    in
    iterate_rolled_hexes rolled_h g

(** [change_victory_pt_limit i g] updates the number of victory points
 *  required to win in [g]. *)
let change_victory_pt_limit (i : int) (g : game_state) : unit =
    g.victory_pt_limit <- i

(** Removes one random resource from [victim] and adds that resource
 *  to the current player's inventory. Does nothing if victim's inventory
 *  is empty. *)
let steal_from  (victim : p_color) (g : game_state) : unit =
    let victim = p_from_color victim g.players in
    let v_inv = victim.p_inventory in
    match List.length victim.p_inventory with
    | 0     ->  print_endline "Nothing to steal"
    | n     ->  let () = Random.self_init() in
                let idx = Random.int n in
                let stolen = List.nth v_inv idx in
                remove_resource victim stolen;
                add_resource g.curr_player stolen;
                print_endline "Resource stolen"

(** Plays a knight development card *)
let play_knight (h_id : int) (pc : p_color) (g : game_state) : unit =
    let p = g.curr_player in
    let can_play =  List.mem Knight p.p_devcards &&
                    g.curr_board.robber_location.h_id <> h_id in
    if can_play then
        let h = h_from_id g.curr_board.hex_tiles h_id in
        g.curr_board.robber_location <- h;
        p.p_knights <- p.p_knights + 1;
        remove_devcard p Knight;
        steal_from pc g;
        update_trophy g;
    else
        raise (Cannot_play "Cannot play knight")

(** Plays a road building development card *)
let play_road_building  (i1 : int) (i2 : int)
                        (g : game_state) : unit =
    let i1 = i_from_id g.curr_board.intersections i1 in
    let i2 = i_from_id g.curr_board.intersections i2 in
    let adj_to_road1 = adjacent_to_road i1 i2 g in
    let adj_to_settle1 = adjacent_to_settlement i1 i2 g in
    let p = g.curr_player in
    let can_play =  List.mem Road_building p.p_devcards &&
                    p.p_road_supply >= 1 &&
                    (* i1 and i2 are adjacent *)
                    List.mem i2.id i1.adj_intersects &&
                    (* a road from i1 to i2 does not already exist *)
                    not (List.mem (i1,i2) g.curr_board.roads ||
                         List.mem (i2,i1) g.curr_board.roads) &&
                    (* potential road 1 is connected to player's network *)
                    (adj_to_road1 || adj_to_settle1)
    in
    if can_play then (
        g.curr_board.roads <- (i1,i2)::g.curr_board.roads;
        p.p_roads <- (i1,i2)::p.p_roads;
        p.p_road_supply <- p.p_road_supply - 1;
        remove_devcard p Road_building;
        print_endline "Road built";
        update_trophy g)
    else
        raise (Cannot_build "Cannot build road")

(** Plays a Year of Plenty card *)
let play_YOP (r1 : resource) (r2 : resource) (g : game_state) : unit =
    let p = g.curr_player in
    let can_play =  List.mem Year_of_plenty p.p_devcards in
    if can_play then (
        add_resource p r1;
        add_resource p r2;
        remove_devcard p Year_of_plenty;)
    else
        raise (Cannot_play "Do not have Year of Plenty card to play")
          
(** Plays a Monopoly card. *)
let play_monopoly (r : resource) (g : game_state) : unit =
    let p = g.curr_player in
    let can_play = List.mem Monopoly p.p_devcards in
    let how_many r p =
        let rs = p.p_inventory in 
        let rec loop r rs acc =
            match rs with
            | []    ->  acc
            | h::t  ->  if h == r then loop r t (acc + 1)
                        else loop r t acc
        in loop r rs 0
    in
    let r_count = List.map (how_many r) g.players in
    let r_count = (List.fold_left (+) 0 r_count) - (how_many r p) in

    if can_play then
        let () = 
            for i = 1 to r_count do
                add_resource p r
            done in
        let rec loop pl =
            match pl with
            | []    ->  ()
            | h::t  ->  let () =
                            while List.mem r h.p_inventory do
                                remove_resource h r;
                            done in
                        loop t
        in 
        let () = 
            print_endline "Monopoly card played";
            remove_devcard p Monopoly;
        in
        loop g.players
    else 
        raise (Cannot_play "No monopoly card to play")

(** Moves robber to specified hex tile *)
let move_robber (h : hex) (g : game_state) : unit =
    let old_h = g.curr_board.robber_location in
    old_h.h_robber_pres <- false;
    g.curr_board.robber_location <- h;
    h.h_robber_pres <- true

(** Removes chosen cards from a player inventory *)
let rec discard_cards   (remove : resource list) (p : p_color) (g : game_state)
                        : unit =
    let player = p_from_color p g.players in
    match remove with
    | []    ->  ()
    | h::t  ->  remove_resource player h; discard_cards t p g

(** Updates curr_player field*)
let end_turn (g : game_state) : unit =
    let next_player =   (match g.curr_player.p_color with
                         | Red   -> (p_from_color Blue g.players)
                         | Blue  -> (p_from_color Green g.players)
                         | Green -> (p_from_color Yellow g.players)
                         | Yellow -> (p_from_color Red g.players)) in
    g.curr_player <- next_player

let rob (g : game_state) : unit =
    let pl = g.players in
    let rec loop pl =
        match pl with
        | []    ->  ()
        | h::t  ->  let rs = List.length h.p_inventory in
                    if rs > 7 then
                        let shuffle_inv = shuffle h.p_inventory in
                        let () =
                            for i = 0 to (rs / 2) - 1 do
                                remove_resource h (List.nth shuffle_inv i);
                            done in
                        print_endline (string_of_pc h.p_color ^ " robbed");
                        loop t

                    else
                        loop t
    in loop pl

(** collect resources from second setup_town. *)
let setup_collect (g : game_state) : unit =
    let ps = g.players in
    let ps = List.map   (fun x -> i_from_id g.curr_board.intersections 
                        (List.hd x.p_settled)) ps in
    let rec thru_il (il : intersection list) : unit =
        match il with
        | h::t  ->  let pl = match h.settlement with 
                            | Some s    -> p_from_color s.owner g.players
                            | None      -> raise Not_found
                    in
                    let rec thru_hl (hl : hex list) (pl : player) : unit =
                        match hl with
                        | []    ->  ()
                        | h::t  ->  add_resource pl h.h_resource;
                                    thru_hl t pl
                    in thru_hl h.adj_hexes pl
        | _     ->  ()
    in thru_il ps


let interp_ai (m : move) (g : game_state) : unit =
  match m with
  | M_DevCard       ->  build_devcard g
  | M_Road (i1,i2)  ->  build_road i1.id i2.id g
  | M_Town i        ->  build_town i.id g
  | M_City i        ->  build_city i.id g
  | M_Trade (r1,r2) ->  bank_trade r1 r2 g
  | M_Port          ->  () (* TODO *)
  | M_EndTurn       ->  end_turn g 

let interp_ai_setup (ms : move * move) (g : game_state) : unit =
  match ms with
  | (M_Town m1, M_Road (m2,m3))  -> setup_town m1.id g;
                                    setup_road m2.id m3.id g
  | _ -> raise (Failure "Cannot handle AI move")
