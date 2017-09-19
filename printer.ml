(* Code written by Al Sandoval *)

open Pervasives
open ANSITerminal
open Game

let populate_settlements (st : game_state) : (int * (style * string)) list =
  let rec fill (i_list : intersection list) (lst : (int * (style * string)) list) =
    match i_list with
    | h::t -> (match h.settlement with
              | None -> fill t ((h.id,(black,"  "))::lst)
              | Some s -> (match s.settle_type with
                            | Town -> (match s.owner with
                                      | Red -> fill t ((h.id,(red,"**"))::lst)
                                      | Blue -> fill t ((h.id,(blue,"**"))::lst)
                                      | Green -> fill t ((h.id,(green,"**"))::lst)
                                      | Yellow -> fill t ((h.id,(yellow,"**"))::lst))
                            | City -> (match s.owner with
                                      | Red -> fill t ((h.id,(red,"[]"))::lst)
                                      | Blue -> fill t ((h.id,(blue,"[]"))::lst)
                                      | Green -> fill t ((h.id,(green,"[]"))::lst)
                                      | Yellow -> fill t ((h.id,(yellow,"[]"))::lst))))
    | [] -> lst
  in
  let board = st.curr_board in
    let i_list = board.intersections in
      fill i_list []

let get_road_id (id1 : int) (id2 : int) : int =
  if ((id1 == 2 && id2 == 3) || (id1 == 3 && id2 == 2)) then
    1
  else  if ((id1 == 1 && id2 == 2) || (id1 == 2 && id2 == 1)) then
    2
  else  if ((id1 == 3 && id2 == 4) || (id1 == 4 && id2 == 3)) then
    3
  else  if ((id1 == 1 && id2 == 30) || (id1 == 30 && id2 == 1)) then
    4
  else  if ((id1 == 4 && id2 == 5) || (id1 == 5 && id2 == 4)) then
    5
  else  if ((id1 == 29 && id2 == 30) || (id1 == 30 && id2 == 29)) then
    6
  else  if ((id1 == 1 && id2 == 31) || (id1 == 31 && id2 == 1)) then
    7
  else  if ((id1 == 4 && id2 == 32) || (id1 == 32 && id2 == 4)) then
    8
  else  if ((id1 == 5 && id2 == 6) || (id1 == 6 && id2 == 5)) then
    9
  else  if ((id1 == 28 && id2 == 29) || (id1 == 29 && id2 == 28)) then
    10
  else  if ((id1 == 31 && id2 == 32) || (id1 == 32 && id2 == 31)) then
    11
  else  if ((id1 == 6 && id2 == 7) || (id1 == 7 && id2 == 6)) then
    12
  else  if ((id1 == 28 && id2 == 27) || (id1 == 27 && id2 == 28)) then
    13
  else  if ((id1 == 29 && id2 == 47) || (id1 == 47 && id2 == 29)) then
    14
  else  if ((id1 == 31 && id2 == 48) || (id1 == 48 && id2 == 31)) then
    15
  else  if ((id1 == 32 && id2 == 33) || (id1 == 33 && id2 == 32)) then
    16
  else  if ((id1 == 6 && id2 == 34) || (id1 == 34 && id2 == 6)) then
    17
  else  if ((id1 == 7 && id2 == 8) || (id1 == 8 && id2 == 7)) then
    18
  else  if ((id1 == 47 && id2 == 48) || (id1 == 48 && id2 == 47)) then
    19
  else  if ((id1 == 33 && id2 == 34) || (id1 == 34 && id2 == 33)) then
    20
  else  if ((id1 == 27 && id2 == 26) || (id1 == 26 && id2 == 27)) then
    21
  else  if ((id1 == 46 && id2 == 47) || (id1 == 47 && id2 == 46)) then
    22
  else  if ((id1 == 48 && id2 == 54) || (id1 == 54 && id2 == 48)) then
    23
  else  if ((id1 == 33 && id2 == 49) || (id1 == 49 && id2 == 33)) then
    24
  else  if ((id1 == 34 && id2 == 35) || (id1 == 35 && id2 == 34)) then
    25
  else  if ((id1 == 8 && id2 == 9) || (id1 == 9 && id2 == 8)) then
    26
  else  if ((id1 == 26 && id2 == 46) || (id1 == 46 && id2 == 26)) then
    27
  else  if ((id1 == 54 && id2 == 49) || (id1 == 49 && id2 == 54)) then
    28
  else  if ((id1 == 35 && id2 == 9) || (id1 == 9 && id2 == 35)) then
    29
  else  if ((id1 == 26 && id2 == 25) || (id1 == 25 && id2 == 26)) then
    30
  else  if ((id1 == 45 && id2 == 46) || (id1 == 46 && id2 == 45)) then
    31
  else  if ((id1 == 54 && id2 == 53) || (id1 == 53 && id2 == 54)) then
    32
  else  if ((id1 == 49 && id2 == 50) || (id1 == 50 && id2 == 49)) then
    33
  else  if ((id1 == 35 && id2 == 36) || (id1 == 36 && id2 == 35)) then
    34
  else  if ((id1 == 9 && id2 == 10) || (id1 == 10 && id2 == 9)) then
    35
  else  if ((id1 == 45 && id2 == 53) || (id1 == 53 && id2 == 45)) then
    36
  else  if ((id1 == 50 && id2 == 36) || (id1 == 36 && id2 == 50)) then
    37
  else  if ((id1 == 25 && id2 == 24) || (id1 == 24 && id2 == 25)) then
    38
  else  if ((id1 == 45 && id2 == 44) || (id1 == 44 && id2 == 45)) then
    39
  else  if ((id1 == 52 && id2 == 53) || (id1 == 53 && id2 == 52)) then
    40
  else  if ((id1 == 50 && id2 == 51) || (id1 == 51 && id2 == 50)) then
    41
  else  if ((id1 == 36 && id2 == 37) || (id1 == 37 && id2 == 36)) then
    42
  else  if ((id1 == 10 && id2 == 11) || (id1 == 11 && id2 == 10)) then
    43
  else  if ((id1 == 24 && id2 == 44) || (id1 == 44 && id2 == 24)) then
    44
  else  if ((id1 == 51 && id2 == 52) || (id1 == 52 && id2 == 51)) then
    45
  else  if ((id1 == 37 && id2 == 11) || (id1 == 11 && id2 == 37)) then
    46
  else  if ((id1 == 24 && id2 == 23) || (id1 == 24 && id2 == 23)) then
    47
  else  if ((id1 == 44 && id2 == 43) || (id1 == 43 && id2 == 44)) then
    48
  else  if ((id1 == 42 && id2 == 52) || (id1 == 52 && id2 == 42)) then
    49
  else  if ((id1 == 51 && id2 == 39) || (id1 == 39 && id2 == 51)) then
    50
  else  if ((id1 == 37 && id2 == 38) || (id1 == 38 && id2 == 37)) then
    51
  else  if ((id1 == 11 && id2 == 12) || (id1 == 12 && id2 == 11)) then
    52
  else  if ((id1 == 42 && id2 == 43) || (id1 == 43 && id2 == 42)) then
    53
  else  if ((id1 == 38 && id2 == 39) || (id1 == 39 && id2 == 38)) then
    54
  else  if ((id1 == 23 && id2 == 22) || (id1 == 22 && id2 == 23)) then
    55
  else  if ((id1 == 21 && id2 == 43) || (id1 == 43 && id2 == 21)) then
    56
  else  if ((id1 == 41 && id2 == 42) || (id1 == 42 && id2 == 41)) then
    57
  else  if ((id1 == 39 && id2 == 40) || (id1 == 40 && id2 == 39)) then
    58
  else  if ((id1 == 38 && id2 == 14) || (id1 == 14 && id2 == 38)) then
    59
  else  if ((id1 == 12 && id2 == 13) || (id1 == 13 && id2 == 12)) then
    60
  else  if ((id1 == 22 && id2 == 21) || (id1 == 21 && id2 == 22)) then
    61
  else  if ((id1 == 40 && id2 == 41) || (id1 == 41 && id2 == 40)) then
    62
  else  if ((id1 == 14 && id2 == 13) || (id1 == 13 && id2 == 14)) then
    63
  else  if ((id1 == 21 && id2 == 20) || (id1 == 20 && id2 == 21)) then
    64
  else  if ((id1 == 19 && id2 == 41) || (id1 == 41 && id2 == 19)) then
    65
  else  if ((id1 == 16 && id2 == 40) || (id1 == 40 && id2 == 16)) then
    66
  else  if ((id1 == 14 && id2 == 15) || (id1 == 15 && id2 == 14)) then
    67
  else  if ((id1 == 19 && id2 == 20) || (id1 == 20 && id2 == 19)) then
    68
  else  if ((id1 == 15 && id2 == 16) || (id1 == 16 && id2 == 15)) then
    69
  else  if ((id1 == 18 && id2 == 19) || (id1 == 19 && id2 == 18)) then
    70
  else  if ((id1 == 16 && id2 == 17) || (id1 == 17 && id2 == 16)) then
    71
  else  if ((id1 == 17 && id2 == 18) || (id1 == 18 && id2 == 17)) then
    72
  else
    -1

let populate_roads (st : game_state) : (int * style) list =
  let pLst = st.players in
  let rec proc_roads (lst : road list) (p : player) : (int * style) list=
  match lst with
  | (i1,i2)::t -> (match p.p_color with
            | Red -> ((get_road_id i1.id i2.id),red)::(proc_roads t p)
            | Blue -> ((get_road_id i1.id i2.id),blue)::(proc_roads t p)
            | Green -> ((get_road_id i1.id i2.id),green)::(proc_roads t p)
            | Yellow -> ((get_road_id i1.id i2.id),yellow)::(proc_roads t p))
  | [] -> []
  in
  let rec proc_players (lst : player list) : (int * style) list =
    match lst with
    | h::t -> (proc_roads h.p_roads h) @ (proc_players t)
    | [] -> []
  in
    proc_players pLst

let get_color (id : int) (lst : (int * style) list) : style =
  try
    List.assoc id lst
  with
  | _ -> black

let populate_hex_resources (st : game_state) : (int * string) list =
  let rec fill (h_list : hex list) (lst : (int * string) list) =
    match h_list with
    | h::t -> (match h.h_resource with
              | Brick -> fill t ((h.h_id,"Brick ")::lst)
              | Lumber -> fill t ((h.h_id,"Lumber")::lst)
              | Ore -> fill t ((h.h_id, " Ore  ")::lst)
              | Wheat -> fill t ((h.h_id,"Wheat ")::lst)
              | Sheep -> fill t ((h.h_id,"Sheep ")::lst)
              | Desert -> fill t ((h.h_id,"Desert")::lst)
              )
    | [] -> lst
  in
  let board = st.curr_board in
    let h_list = board.hex_tiles in
      fill h_list []

let populate_hex_numbers (st : game_state) : (int * string) list =
  let rec fill (h_list : hex list) (lst : (int * string) list) =
    match h_list with
    | h::t -> if (h.h_roll_number > 9) then
              fill t ((h.h_id,(string_of_int h.h_roll_number))::lst)
              else
              fill t ((h.h_id,(string_of_int h.h_roll_number)^" ")::lst)
    | [] -> lst
  in
  let board = st.curr_board in
    let h_list = board.hex_tiles in
      fill h_list []

let rec populate_hncolors numList : (int * style) list =
  match numList with
  | (id,number)::t -> if(number = "6 " || number = "8 ") then
                      (id,red) :: populate_hncolors t
                      else if(number = "2 " || number = "12") then
                      (id,blue) :: populate_hncolors t
                      else if(number = "0 ") then
                      (id,white) :: populate_hncolors t
                      else
                      (id,black) :: populate_hncolors t
  | [] -> []

let print_board (st : game_state) : unit =

  let h_r = populate_hex_resources st in
  let h_n = populate_hex_numbers st in
  let n_c = populate_hncolors h_n in
  let i_s = populate_settlements st in
  let r = populate_roads st in

  ANSITerminal.(print_string [black] ("                                                                         "));   ANSITerminal.(print_string [black] ("                                                                          \n"));
  ANSITerminal.(print_string [black] ("                                                                         "));   ANSITerminal.(print_string [black] ("                                                                          \n"));
  ANSITerminal.(print_string [black] ("                           ")); ANSITerminal.(print_string [(fst(List.assoc 2 i_s))] (snd(List.assoc 2 i_s))); ANSITerminal.(print_string [get_color 1 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 3 i_s))] (snd(List.assoc 3 i_s))); ANSITerminal.(print_string [black] ("                                "));   ANSITerminal.(print_string [black] ("                            2______3                                      \n"));
  ANSITerminal.(print_string [black] ("                            ")); ANSITerminal.(print_string [get_color 2 r] ("/")); ANSITerminal.(print_string [get_color 3 r] ("      \\                                     "));   ANSITerminal.(print_string [black] ("                        /      \\                                     \n"));
  ANSITerminal.(print_string [black] ("                           ")); ANSITerminal.(print_string [get_color 2 r] ("/")); ANSITerminal.(print_string [get_color 3 r] ("        \\                                    "));   ANSITerminal.(print_string [black] ("                       /        \\                                    \n"));
  ANSITerminal.(print_string [black] ("               ")); ANSITerminal.(print_string [(fst(List.assoc 30 i_s))] (snd(List.assoc 30 i_s))); ANSITerminal.(print_string [get_color 4 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 1 i_s))] (snd(List.assoc 1 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 1 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 4 i_s))] (snd(List.assoc 4 i_s))); ANSITerminal.(print_string [get_color 5 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 5 i_s))] (snd(List.assoc 5 i_s))); ANSITerminal.(print_string [black] ("                    "));   ANSITerminal.(print_string [black] ("               30______1  |     1    |  4______5                          \n"));
  ANSITerminal.(print_string [black] ("                ")); ANSITerminal.(print_string [get_color 6 r] "/      "); ANSITerminal.(print_string [get_color 7 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 1 n_c] ("   "^(List.assoc 1 h_n)^"   ")); ANSITerminal.(print_string [get_color 8 r] "/   /"); ANSITerminal.(print_string [get_color 9 r] "      \\                       ");   ANSITerminal.(print_string [black] ("              /      \\   \\        /   /      \\                       \n"));
  ANSITerminal.(print_string [black] ("               ")); ANSITerminal.(print_string [get_color 6 r] "/        "); ANSITerminal.(print_string [get_color 7 r] "\\   \\"); ANSITerminal.(print_string [get_color 11 r] "______"); ANSITerminal.(print_string [get_color 8 r] "/   /"); ANSITerminal.(print_string [get_color 9 r] "        \\                      ");   ANSITerminal.(print_string [black] ("             /        \\   \\______/   /        \\                      \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [(fst(List.assoc 28 i_s))] (snd(List.assoc 28 i_s))); ANSITerminal.(print_string [get_color 10 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 29 i_s))] (snd(List.assoc 29 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 12 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 31 i_s))] (snd(List.assoc 31 i_s))); ANSITerminal.(print_string [get_color 11 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 32 i_s))] (snd(List.assoc 32 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 2 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 6 i_s))] (snd(List.assoc 6 i_s))); ANSITerminal.(print_string [get_color 12 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 7 i_s))] (snd(List.assoc 7 i_s))); ANSITerminal.(print_string [black] ("        "));   ANSITerminal.(print_string [black] ("   28______29 |    12    | 31______32 |    2     |  6______7              \n"));
  ANSITerminal.(print_string [black] ("    ")); ANSITerminal.(print_string [get_color 13 r] "/"); ANSITerminal.(print_string [get_color 14 r] "      \\   \\"); ANSITerminal.(print_string [List.assoc 12 n_c] ("   "^(List.assoc 12 h_n)^"   ")); ANSITerminal.(print_string [get_color 15 r] "/   /      "); ANSITerminal.(print_string [get_color 16 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 2 n_c] ("   "^(List.assoc 2 h_n)^"   ")); ANSITerminal.(print_string [get_color 17 r] "/   /"); ANSITerminal.(print_string [get_color 18 r] "      \\         ");   ANSITerminal.(print_string [black] ("    /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 13 r] "/        "); ANSITerminal.(print_string [get_color 14 r] "\\   \\"); ANSITerminal.(print_string [get_color 19 r] "______"); ANSITerminal.(print_string [get_color 15 r] "/   /"); ANSITerminal.(print_string [get_color 16 r] "        \\   \\"); ANSITerminal.(print_string [get_color 20 r] "______"); ANSITerminal.(print_string [get_color 17 r] "/   /"); ANSITerminal.(print_string [get_color 18 r] "        \\        ");   ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("")); ANSITerminal.(print_string [(fst(List.assoc 27 i_s))] (snd(List.assoc 27 i_s))); ANSITerminal.(print_string [black] ("|  "^(List.assoc 11 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 47 i_s))] (snd(List.assoc 47 i_s))); ANSITerminal.(print_string [get_color 19 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 48 i_s))] (snd(List.assoc 48 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 13 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 33 i_s))] (snd(List.assoc 33 i_s))); ANSITerminal.(print_string [get_color 20 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 34 i_s))] (snd(List.assoc 34 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 3 h_r)^"  |")); ANSITerminal.(print_string [(fst(List.assoc 8 i_s))] (snd(List.assoc 8 i_s))); ANSITerminal.(print_string [black] ("     "));   ANSITerminal.(print_string [black] ("27|    11    | 47______48 |   13     | 33______34 |    3     |8           \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 21 r] "\\"); ANSITerminal.(print_string [List.assoc 11 n_c] ("   "^(List.assoc 11 h_n)^"   ")); ANSITerminal.(print_string [get_color 22 r] "/   /"); ANSITerminal.(print_string [get_color 23 r] "      \\   \\"); ANSITerminal.(print_string [List.assoc 13 n_c] ("   "^(List.assoc 13 h_n)^"   ")); ANSITerminal.(print_string [get_color 24 r] "/   /      "); ANSITerminal.(print_string [get_color 25 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 3 n_c] ("   "^(List.assoc 3 h_n)^"   ")); ANSITerminal.(print_string [get_color 26 r] "/        ");   ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 21 r] " \\"); ANSITerminal.(print_string [get_color 27 r] "______"); ANSITerminal.(print_string [get_color 22 r] "/   /"); ANSITerminal.(print_string [get_color 23 r] "        \\   \\"); ANSITerminal.(print_string [get_color 28 r] "______"); ANSITerminal.(print_string [get_color 24 r] "/   /"); ANSITerminal.(print_string [get_color 25 r] "        \\   \\"); ANSITerminal.(print_string [get_color 29 r] "______"); ANSITerminal.(print_string [get_color 26 r] "/         ");   ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [(fst(List.assoc 26 i_s))] (snd(List.assoc 26 i_s))); ANSITerminal.(print_string [get_color 27 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 46 i_s))] (snd(List.assoc 46 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 18 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 54 i_s))] (snd(List.assoc 54 i_s))); ANSITerminal.(print_string [get_color 28 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 49 i_s))] (snd(List.assoc 49 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 14 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 35 i_s))] (snd(List.assoc 35 i_s))); ANSITerminal.(print_string [get_color 29 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 9 i_s))] (snd(List.assoc 9 i_s))); ANSITerminal.(print_string [black] ("        "));   ANSITerminal.(print_string [black] ("   26______ 46|   18     | 54______49 |    14    | 35______9              \n"));
  ANSITerminal.(print_string [black] ("    ")); ANSITerminal.(print_string [get_color 30 r] "/      "); ANSITerminal.(print_string [get_color 31 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 18 n_c] ("   "^(List.assoc 18 h_n)^"   ")); ANSITerminal.(print_string [get_color 32 r] "/   /      "); ANSITerminal.(print_string [get_color 33 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 14 n_c] ("   "^(List.assoc 14 h_n)^"   ")); ANSITerminal.(print_string [get_color 34 r] "/   /      "); ANSITerminal.(print_string [get_color 35 r] "\\ ");   ANSITerminal.(print_string [black] ("            /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 30 r] "/        "); ANSITerminal.(print_string [get_color 31 r] "\\   \\"); ANSITerminal.(print_string [get_color 36 r] "______"); ANSITerminal.(print_string [get_color 32 r] "/   /"); ANSITerminal.(print_string [get_color 33 r] "        \\   \\"); ANSITerminal.(print_string [get_color 37 r] "______"); ANSITerminal.(print_string [get_color 34 r] "/   /"); ANSITerminal.(print_string [get_color 35 r] "        \\        ");   ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("")); ANSITerminal.(print_string [(fst(List.assoc 25 i_s))] (snd(List.assoc 25 i_s))); ANSITerminal.(print_string [black] ("|  "^(List.assoc 10 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 45 i_s))] (snd(List.assoc 45 i_s))); ANSITerminal.(print_string [get_color 36 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 53 i_s))] (snd(List.assoc 53 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 19 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 50 i_s))] (snd(List.assoc 50 i_s))); ANSITerminal.(print_string [get_color 37 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 36 i_s))] (snd(List.assoc 36 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 4 h_r)^"  |")); ANSITerminal.(print_string [(fst(List.assoc 10 i_s))] (snd(List.assoc 10 i_s))); ANSITerminal.(print_string [black] ("     "));   ANSITerminal.(print_string [black] ("25|    10    | 45______53 |    19    | 50______36 |    4     |10          \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 38 r] "\\"); ANSITerminal.(print_string [List.assoc 10 n_c] ("   "^(List.assoc 10 h_n)^"   ")); ANSITerminal.(print_string [get_color 39 r] "/   /"); ANSITerminal.(print_string [get_color 40 r] "      \\   \\"); ANSITerminal.(print_string [List.assoc 19 n_c] ("   "^(List.assoc 19 h_n)^"   ")); ANSITerminal.(print_string [get_color 41 r] "/   /      "); ANSITerminal.(print_string [get_color 42 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 4 n_c] ("   "^(List.assoc 4 h_n)^"   ")); ANSITerminal.(print_string [get_color 43 r] "/        ");   ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 38 r] " \\"); ANSITerminal.(print_string [get_color 44 r] "______"); ANSITerminal.(print_string [get_color 39 r] "/   /"); ANSITerminal.(print_string [get_color 40 r] "        \\   \\"); ANSITerminal.(print_string [get_color 45 r] "______"); ANSITerminal.(print_string [get_color 41 r] "/   /"); ANSITerminal.(print_string [get_color 42 r] "        \\   \\"); ANSITerminal.(print_string [get_color 46 r] "______"); ANSITerminal.(print_string [get_color 43 r] "/         ");   ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [(fst(List.assoc 24 i_s))] (snd(List.assoc 24 i_s))); ANSITerminal.(print_string [get_color 44 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 44 i_s))] (snd(List.assoc 44 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 17 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 52 i_s))] (snd(List.assoc 52 i_s))); ANSITerminal.(print_string [get_color 45 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 51 i_s))] (snd(List.assoc 51 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 15 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 37 i_s))] (snd(List.assoc 37 i_s))); ANSITerminal.(print_string [get_color 46 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 11 i_s))] (snd(List.assoc 11 i_s))); ANSITerminal.(print_string [black] ("        "));   ANSITerminal.(print_string [black] ("   24______ 44|    17    |52 ______ 51|    15    | 37______11             \n"));
  ANSITerminal.(print_string [black] ("    ")); ANSITerminal.(print_string [get_color 47 r] "/      "); ANSITerminal.(print_string [get_color 48 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 17 n_c] ("   "^(List.assoc 17 h_n)^"   ")); ANSITerminal.(print_string [get_color 49 r] "/   /      "); ANSITerminal.(print_string [get_color 50 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 15 n_c] ("   "^(List.assoc 15 h_n)^"   ")); ANSITerminal.(print_string [get_color 51 r] "/   /      "); ANSITerminal.(print_string [get_color 52 r] "\\ ");   ANSITerminal.(print_string [black] ("            /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 47 r] "/        "); ANSITerminal.(print_string [get_color 48 r] "\\   \\"); ANSITerminal.(print_string [get_color 53 r] "______"); ANSITerminal.(print_string [get_color 49 r] "/   /"); ANSITerminal.(print_string [get_color 50 r] "        \\   \\"); ANSITerminal.(print_string [get_color 54 r] "______"); ANSITerminal.(print_string [get_color 51 r] "/   /"); ANSITerminal.(print_string [get_color 52 r] "        \\        ");   ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("")); ANSITerminal.(print_string [(fst(List.assoc 23 i_s))] (snd(List.assoc 23 i_s))); ANSITerminal.(print_string [black] ("|  "^(List.assoc 9 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 43 i_s))] (snd(List.assoc 43 i_s))); ANSITerminal.(print_string [get_color 53 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 42 i_s))] (snd(List.assoc 42 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 16 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 39 i_s))] (snd(List.assoc 39 i_s))); ANSITerminal.(print_string [get_color 54 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 38 i_s))] (snd(List.assoc 38 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 5 h_r)^"  |")); ANSITerminal.(print_string [(fst(List.assoc 12 i_s))] (snd(List.assoc 12 i_s))); ANSITerminal.(print_string [black] ("     "));   ANSITerminal.(print_string [black] ("23|    9     | 43______42 |    16    | 39______38 |    5     |12          \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 55 r] "\\"); ANSITerminal.(print_string [List.assoc 9 n_c] ("   "^(List.assoc 9 h_n)^"   ")); ANSITerminal.(print_string [get_color 56 r] "/   /"); ANSITerminal.(print_string [get_color 57 r] "      \\   \\"); ANSITerminal.(print_string [List.assoc 16 n_c] ("   "^(List.assoc 16 h_n)^"   ")); ANSITerminal.(print_string [get_color 58 r] "/   /      "); ANSITerminal.(print_string [get_color 59 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 5 n_c] ("   "^(List.assoc 5 h_n)^"   ")); ANSITerminal.(print_string [get_color 60 r] "/        ");   ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [get_color 55 r] " \\"); ANSITerminal.(print_string [get_color 61 r] "______"); ANSITerminal.(print_string [get_color 56 r] "/   /"); ANSITerminal.(print_string [get_color 57 r] "        \\   \\"); ANSITerminal.(print_string [get_color 62 r] "______"); ANSITerminal.(print_string [get_color 58 r] "/   /"); ANSITerminal.(print_string [get_color 59 r] "        \\   \\"); ANSITerminal.(print_string [get_color 63 r] "______"); ANSITerminal.(print_string [get_color 60 r] "/         ");   ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   ")); ANSITerminal.(print_string [(fst(List.assoc 22 i_s))] (snd(List.assoc 22 i_s))); ANSITerminal.(print_string [black] ("      ")); ANSITerminal.(print_string [(fst(List.assoc 21 i_s))] (snd(List.assoc 21 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 8 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 41 i_s))] (snd(List.assoc 41 i_s))); ANSITerminal.(print_string [get_color 62 r] ("______")); ANSITerminal.(print_string [(fst(List.assoc 40 i_s))] (snd(List.assoc 40 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 6 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 14 i_s))] (snd(List.assoc 14 i_s))); ANSITerminal.(print_string [black] ("      ")); ANSITerminal.(print_string [(fst(List.assoc 13 i_s))] (snd(List.assoc 13 i_s))); ANSITerminal.(print_string [black] ("        "));   ANSITerminal.(print_string [black] ("   22       21|    8     |41 ______ 40|   6      |14        13            \n"));
  ANSITerminal.(print_string [black] ("               ")); ANSITerminal.(print_string [get_color 64 r] "\\"); ANSITerminal.(print_string [List.assoc 8 n_c] ("   "^(List.assoc 8 h_n)^"   ")); ANSITerminal.(print_string [get_color 65 r] "/   /      "); ANSITerminal.(print_string [get_color 66 r] "\\   \\"); ANSITerminal.(print_string [List.assoc 6 n_c] ("   "^(List.assoc 6 h_n)^"   ")); ANSITerminal.(print_string [get_color 67 r] "/                    ");   ANSITerminal.(print_string [black] ("               \\        /   /      \\   \\        /                      \n"));
  ANSITerminal.(print_string [black] ("                ")); ANSITerminal.(print_string [get_color 64 r] "\\"); ANSITerminal.(print_string [get_color 68 r] "______"); ANSITerminal.(print_string [get_color 65 r] "/   /"); ANSITerminal.(print_string [get_color 66 r] "        \\   \\"); ANSITerminal.(print_string [get_color 69 r] "______"); ANSITerminal.(print_string [get_color 67 r] "/                     ");   ANSITerminal.(print_string [black] ("                \\______/   /        \\   \\______/                       \n"));
  ANSITerminal.(print_string [black] ("               ")); ANSITerminal.(print_string [(fst(List.assoc 20 i_s))] (snd(List.assoc 20 i_s))); ANSITerminal.(print_string [black] ("      ")); ANSITerminal.(print_string [(fst(List.assoc 19 i_s))] (snd(List.assoc 19 i_s))); ANSITerminal.(print_string [black] (" |  "^(List.assoc 7 h_r)^"  | ")); ANSITerminal.(print_string [(fst(List.assoc 16 i_s))] (snd(List.assoc 16 i_s))); ANSITerminal.(print_string [black] ("      ")); ANSITerminal.(print_string [(fst(List.assoc 15 i_s))] (snd(List.assoc 15 i_s))); ANSITerminal.(print_string [black] ("                    "));   ANSITerminal.(print_string [black] ("               20      19 |    7     | 16      15                         \n"));
  ANSITerminal.(print_string [black] ("                           ")); ANSITerminal.(print_string [get_color 70 r] "\\"); ANSITerminal.(print_string [List.assoc 7 n_c] ("   "^(List.assoc 7 h_n)^"   ")); ANSITerminal.(print_string [get_color 71 r] "/                                ");   ANSITerminal.(print_string [black] ("                           \\        /                                    \n"));
  ANSITerminal.(print_string [black] ("                          ")); ANSITerminal.(print_string [(fst(List.assoc 18 i_s))] (snd(List.assoc 18 i_s))); ANSITerminal.(print_string [get_color 70 r] "\\"); ANSITerminal.(print_string [get_color 72 r] "______"); ANSITerminal.(print_string [get_color 71 r] "/"); ANSITerminal.(print_string [(fst(List.assoc 17 i_s))] (snd(List.assoc 17 i_s))); ANSITerminal.(print_string [black] ("                               "));   ANSITerminal.(print_string [black] ("                            \\______/                                     \n"));
  ANSITerminal.(print_string [black] ("                                                                          "));   ANSITerminal.(print_string [black] ("                     18        17                                    \n"));
  ANSITerminal.(print_string [black] ("                                                                          \n\n"))

let print_IDs () =
  ANSITerminal.(print_string [black] ("                                                                          \n"));
  ANSITerminal.(print_string [black] ("                                                                          \n"));
  ANSITerminal.(print_string [black] ("                            2______3                                      \n"));
  ANSITerminal.(print_string [black] ("                            /      \\                                     \n"));
  ANSITerminal.(print_string [black] ("                           /        \\                                    \n"));
  ANSITerminal.(print_string [black] ("               30______1  |     1    |  4______5                          \n"));
  ANSITerminal.(print_string [black] ("                /      \\   \\        /   /      \\                       \n"));
  ANSITerminal.(print_string [black] ("               /        \\   \\______/   /        \\                      \n"));
  ANSITerminal.(print_string [black] ("   28______29 |    12    | 31______32 |    2     |  6______7              \n"));
  ANSITerminal.(print_string [black] ("    /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("27|    11    | 47______48 |   13     | 33______34 |    3     |8           \n"));
  ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   26______ 46|   18     | 54______49 |    14    | 35______9              \n"));
  ANSITerminal.(print_string [black] ("    /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("25|    10    | 45______53 |    19    | 50______36 |    4     |10          \n"));
  ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   24______ 44|    17    |52 ______ 51|    15    | 37______11             \n"));
  ANSITerminal.(print_string [black] ("    /      \\   \\        /   /      \\   \\        /   /      \\         \n"));
  ANSITerminal.(print_string [black] ("   /        \\   \\______/   /        \\   \\______/   /        \\        \n"));
  ANSITerminal.(print_string [black] ("23|    9     | 43______42 |    16    | 39______38 |    5     |12          \n"));
  ANSITerminal.(print_string [black] ("   \\        /   /      \\   \\        /   /      \\   \\        /        \n"));
  ANSITerminal.(print_string [black] ("    \\______/   /        \\   \\______/   /        \\   \\______/         \n"));
  ANSITerminal.(print_string [black] ("   22       21|    8     |41 ______ 40|   6      |14        13            \n"));
  ANSITerminal.(print_string [black] ("               \\        /   /      \\   \\        /                      \n"));
  ANSITerminal.(print_string [black] ("                \\______/   /        \\   \\______/                       \n"));
  ANSITerminal.(print_string [black] ("               20       19|    7     |16       15                         \n"));
  ANSITerminal.(print_string [black] ("                           \\        /                                    \n"));
  ANSITerminal.(print_string [black] ("                            \\______/                                     \n"));
  ANSITerminal.(print_string [black] ("                          18        17                                    \n"));
  ANSITerminal.(print_string [black] ("                                                                          \n"))

let print_points (st : game_state) (p : player) : unit =
  ANSITerminal.(print_string [red] "Leaderboard:\n");
  let rec print_player pLst =
    match pLst with
    | h::t -> ANSITerminal.(print_string [red] 
              ((string_of_pc h.p_color) ^ " has " ^ (string_of_int h.p_victorypts) ^ " victory points.\n"));
              print_player t
    | [] -> ANSITerminal.(print_string [red] "\n")
  in print_player st.players


let print_inventory (st : game_state) (p : player) : unit =
      if(p.p_inventory = [] && p.p_devcards = []) then (
        ANSITerminal.(print_string [red] "\n");
        ANSITerminal.(print_string [red] "Your inventory is empty!\n"))
      else(
        ANSITerminal.(print_string [red] "\n");
        ANSITerminal.(print_string [red] (string_of_dl (p.p_devcards)));
        ANSITerminal.(print_string [red] "\n");
        ANSITerminal.(print_string [red] (string_of_rl (p.p_inventory)));
        ANSITerminal.(print_string [red] ("\n")))
