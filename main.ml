open Game
open Ai
open Printer

(**	[quit st] quits the game if it recieves confirmation and returns unit
 *  otherwise. *)
let quit () : unit = 
  print_endline "Are you sure you want to quit? Progress will be lost (y/n) ";
  ANSITerminal.(print_string [red] "> ");
  let str = read_line () in
  let str = String.lowercase_ascii str in
  if String.length str > 0 && str.[0] = 'y' then exit 0
  else ()

let to_res str =
match str with
  | "brick" -> Brick | "lumber" -> Lumber | "ore" -> Ore 
  | "wheat" -> Wheat | "sheep" -> Sheep  | _ -> failwith "not valid resource"

let rec placement_loop st =

  let () = print_board st in
  let rec try_setup_t idt = 
    try setup_town (int_of_string(idt)) st 
    with | _ -> (ANSITerminal.(print_string [red] "Try again. Where do you want to place your town?\n> "));
                try_setup_t (String.lowercase_ascii (read_line()))
  in

  let rec try_setup_r id1 id2 g =  
    try setup_road (int_of_string(id1)) (int_of_string(id2)) st 
    with | _ -> (ANSITerminal.(print_string [red] "Invalid road location(s), please try again\n> "));
                (ANSITerminal.(print_string [red] "Where would you like your to road to go?\n> "));
                let id1 = String.lowercase_ascii (read_line()) in
                try_setup_r id1 (string_of_int (List.hd st.curr_player.p_settled)) g
  in 

  (* If AI, get move from AI *)
  if st.curr_player.p_ai then (
    let () = ANSITerminal.(print_string [red] (string_of_pc (st.curr_player.p_color) ^ " AI's turn.\n")) in
    let ai_init = init_move st in
    let () = interp_ai_setup ai_init st in
    Unix.sleepf 0.8)
  (* Else, get move from input *)
  else ( 
    ANSITerminal.(print_string [red] ("\nIt is now "^string_of_pc (st.curr_player.p_color) ^ "'s turn.\n"));
    ANSITerminal.(print_string [red] "When you are ready, type in the ID of where you would like to place your town.\n> ");
    let idt = String.lowercase_ascii (read_line()) in
    let () = try_setup_t idt; print_board st in 
    ANSITerminal.(print_string [red] "On which adjacent intersection would you like to build your road?\n> ");
    let id1 = String.lowercase_ascii (read_line()) in
    try_setup_r id1 (string_of_int (List.hd st.curr_player.p_settled))) st
    
let init_repl () =
  let st = game_init () in
    placement_loop st;
    end_turn st;
    placement_loop st;
    end_turn st;
    placement_loop st;
    end_turn st;
    placement_loop st;
    end_turn st; end_turn st; end_turn st; end_turn st;
    placement_loop st;
    end_turn st; end_turn st; end_turn st;
    placement_loop st;
    end_turn st; end_turn st; end_turn st;
    placement_loop st;
    end_turn st; end_turn st; end_turn st;
    placement_loop st;
    setup_collect st;
    st

let do' cmd st =
  let () = print_board st in
  if cmd = "" then (
    print_board st;
    ANSITerminal.(print_string [red] "> ");
    st)
  else if (cmd = "end" || cmd = "1") then
    let () = end_turn st in 
    let roll = roll_dice () in
    ANSITerminal.(print_string [red] ((string_of_pc st.curr_player.p_color) ^ " rolls " ^ string_of_int(roll)^".\n"));
    print_board st;
    if roll == 7 then (
      rob st;
      st)
    else (
      collect_r roll st;
      st
      )
  else if (cmd = "inv"|| cmd = "2" || cmd = "inventory") then
    (print_inventory st st.curr_player;
     st
    )
  else if (cmd = "trade"|| cmd = "3") then
    (ANSITerminal.(print_string [red] ("What resource of yours would you like to trade?\n> "));
    let give = String.lowercase_ascii (read_line()) in
    ANSITerminal.(print_string [red] ("What resource would you like to recieve\n> "));
    let recieve = String.lowercase_ascii (read_line()) in
    try
      bank_trade (to_res(give)) (to_res(recieve)) st;
      ANSITerminal.(print_string [red] "Congratulations! Trade Succesful.\n> ");
      st
    with
    | _ -> ANSITerminal.(print_string [red] "Trade Invalid.\n> ");
            st
    )
  else if (cmd = "build town" || cmd = "4") then
    (ANSITerminal.(print_string [red] ("What intersection would you like to build?\n> "));
    let id = String.lowercase_ascii (read_line()) in
    try
      build_town (int_of_string(id)) st;
      print_board st;
      ANSITerminal.(print_string [red] "Congratulations! Build Succesful.\n> ");
      st
    with
    | _ -> ANSITerminal.(print_string [red] "Build Invalid.\n> ");
            st
    )  
  else if (cmd = "build road" || cmd = "5") then
    (ANSITerminal.(print_string [red] ("First intersection you would like to build?\n> "));
    let id1 = String.lowercase_ascii (read_line()) in
    ANSITerminal.(print_string [red] ("Second intersection you would like to build?\n> "));
    let id2 = String.lowercase_ascii (read_line()) in
    try
      build_road (int_of_string(id1)) (int_of_string(id2)) st;
      print_board st;
      ANSITerminal.(print_string [red] "Congratulations! Build Succesful.\n> ");
      st
    with
    | _ -> ANSITerminal.(print_string [red] "Build Invalid.\n> ");
           st
    )
  else if (cmd = "build city" || cmd = "6") then
    (ANSITerminal.(print_string [red] ("What intersection would you like to build?\n> "));
    let id = String.lowercase_ascii (read_line()) in
    try
      build_city (int_of_string(id)) st;
      print_board st;
      ANSITerminal.(print_string [red] "Congratulations! Build Succesful.\n> ");
      st
    with
    | _ -> ANSITerminal.(print_string [red] "Build Invalid.\n> ");
            st
    )
  else if (cmd = "dev card" || cmd = "7") then
    try
      build_devcard st;
      ANSITerminal.(print_string [red] "Congratulations! Development card built.\n> ");
      st
    with
    | _ -> ANSITerminal.(print_string [red] "Build Invalid.\n> ");
            st
  else if (cmd = "exit" || cmd = "11" || cmd = "quit") then
    (quit ();
     st
    )
  else if (cmd = "play card" || cmd = "8") then
    (ANSITerminal.(print_string [red] ("What type of card would you like to play?\n"^
          "(K for Knight, M for Monopoly, R for Road Building or Y for Year of Plenty\n
                                        > "));
    let card_in = String.lowercase_ascii (read_line()) in
      match card_in with
      | "k" -> (ANSITerminal.(print_string [red] "\nWhere would you like to move the robber? (hex id)\n> ");
                let id = int_of_string (read_line()) in
                ANSITerminal.(print_string [red] ("\nWhich player would you like to steal from?\n"^
                                                  "(Blue, Green, Yellow)\n> "));
                let col = String.lowercase_ascii (read_line()) in
                try
                  match col with
                  | "blue" -> play_knight id Blue st;
                              st
                  | "green" -> play_knight id Green st;
                              st
                  | "yellow" -> play_knight id Yellow st;
                              st
                  | _ -> ANSITerminal.(print_string [red] "\nInvalid player");
                        st
                with
                | _ -> ANSITerminal.(print_string [red] "\nInvalid player or id");
                        st)
      | "m" -> (ANSITerminal.(print_string [red] ("What resource would you like to monopolize?\n> "));
                let res = String.lowercase_ascii (read_line()) in
                try
                  play_monopoly (to_res(res)) st;
                  st
                with
                | _ -> ANSITerminal.(print_string [red] "Invalid play.");
                        st
                )
      | "r" -> (print_board st;
                ANSITerminal.(print_string [red] ("First intersection you would like to build?\n> "));
                let id1 = String.lowercase_ascii (read_line()) in
                ANSITerminal.(print_string [red] ("Second intersection you would like to build?\n> "));
                let id2 = String.lowercase_ascii (read_line()) in
                try
                  build_road (int_of_string(id1)) (int_of_string(id2)) st;
                  print_board st;
                  ANSITerminal.(print_string [red] "Congratulations! Build Succesful.\n> ");
                  st
                with
                | _ -> ANSITerminal.(print_string [red] "Build Invalid.\n> ");
                st) 
      | "y" ->     (ANSITerminal.(print_string [red] ("First resource you would like?\n> "));
                    let r1 = String.lowercase_ascii (read_line()) in
                    ANSITerminal.(print_string [red] ("Second resource you would like?\n> "));
                    let r2 = String.lowercase_ascii (read_line()) in
                    try
                      play_YOP (to_res(r1)) (to_res(r2)) st;
                      ANSITerminal.(print_string [red] "Congratulations! Resources added.\n> ");
                      st
                    with
                    | _ -> ANSITerminal.(print_string [red] "Invalid play.\n> ");
                           st)
      | _ -> ANSITerminal.(print_string [red] "Invalid card selection.");
              st
    )  
  else if (cmd = "points" || cmd = "9") then
    (print_points st st.curr_player;
    st
    )
  else if (cmd = "print" || cmd = "10") then
    (print_board st;
    st
    )

  else if (cmd = "help") then (
    ANSITerminal.(print_string [red] "Enter 1 or \"end\" to end turn\n");
    ANSITerminal.(print_string [red] "Enter 2 or \"inv\" to print inventory\n");
    ANSITerminal.(print_string [red] "Enter 3 or \"trade\" to engage in a trade with the bank (4 of any resource for 1 of any)\n");
    ANSITerminal.(print_string [red] "Enter 4 or \"build town\" to build a town (costs 1 sheep, 1 lumber, 1 brick, 1 wheat)\n");
    ANSITerminal.(print_string [red] "Enter 5 or \"build road\" to build a road (costs 1 lumber, 1 brick)\n");
    ANSITerminal.(print_string [red] "Enter 6 or \"build city\" to build a city on an existing town (costs 2 wheat, 3 ore)\n");
    ANSITerminal.(print_string [red] "Enter 7 or \"dev card\" to build a development card (costs 1 sheep, 1 wheat, 1 ore)\n");
    ANSITerminal.(print_string [red] "Enter 8 or \"play card\" to play a development card\n");
    ANSITerminal.(print_string [red] "Enter 9 or \"points\" to print out every player's points\n");
    ANSITerminal.(print_string [red] "Enter 10 or \"print\" print board\n");
    ANSITerminal.(print_string [red] "Enter 11 or \"quit\" to quit\n\n> ");
    st)
  else
    (ANSITerminal.(print_string [red] "Not a valid command\n> ");
    st)

let sleep (n : float) : unit = Unix.sleepf n

let rec repl st =
  try
  if st.curr_player.p_ai then (
    let ai_move = try main st with | _ -> M_EndTurn in
    if ai_move == M_EndTurn then (
      let roll = roll_dice () in
      let () = interp_ai ai_move st in
      let () = sleep 0.5 in
      ANSITerminal.(print_string [red] ((string_of_pc st.curr_player.p_color) ^ "'s turn\n"));
      ANSITerminal.(print_string [red] ((string_of_pc st.curr_player.p_color) ^ " rolls " ^ string_of_int(roll)^".\n"));
      let () = (
        if roll == 7 then 
          rob st
        else
          collect_r roll st)
      in
      print_board st;
      repl st)
    else (
      if List.length st.curr_player.p_devcards > 0 then
        let card = List.hd st.curr_player.p_devcards in
        let () =
        match card with
        | Knight ->   let (pc,h) = knight st in
                      let () = try play_knight h.h_id pc st with | _ -> () in
                      ()
        | Road_building ->  let (i1,i2) = road_build st in
                            play_road_building i1.id i2.id st;
                            let (i3,i4) = road_build st in
                            play_road_building i3.id i4.id st
        | Year_of_plenty ->   let (r1,r2) = year_of_plenty st in
                              play_YOP r1 r2 st
        | Monopoly    ->  let r = monopoly st in
                          play_monopoly r st
        | _ -> raise Not_found in

      let () = interp_ai ai_move st in
      let () = sleep 0.8 in
      print_board st;
      repl st
    else (
      let () = interp_ai ai_move st in
      let () = sleep 0.8 in
      print_board st;
      repl st)
    ))
  else (
    ANSITerminal.(print_string [red] ("The robber is currently on hex: " ^
                    (string_of_int(st.curr_board.robber_location.h_id))));
    print_inventory st st.curr_player;
    ANSITerminal.(print_string [red] ((string_of_pc st.curr_player.p_color) ^ "'s turn. Type \"help\" for a list of commands.\n> "));
    (repl (do' (String.lowercase_ascii (read_line())) st)))
  with | _ -> end_turn st; repl st

let () =
  	ANSITerminal.(print_string [red] 
    	"\n\nWelcome to Settlers of Catan.\n");
    ANSITerminal.(print_string [red] 
      "\nPress return to begin\n\n");
    let () = ignore (read_line()) in
    ANSITerminal.(print_string [red] "\nThis is the initial phase, each player will place two roads and two towns.\n");
  	let g = init_repl () in
    ANSITerminal.(print_string [red] "\nYour adventure has started, type \"help\" for a list of commands\n");
      let roll = roll_dice () in
      ANSITerminal.(print_string [red] ((string_of_pc g.curr_player.p_color) ^ " rolls " ^ string_of_int(roll)^".\n"));
      print_board g;
      if roll == 7 then (
        (* call robber function *)
        ();
        repl g)
      else(
        collect_r roll g;
        repl g)
