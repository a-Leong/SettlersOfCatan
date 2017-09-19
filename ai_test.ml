open OUnit2
open Game
open Ai



let gamestate = game_init ()
let intersections= gamestate.curr_board.intersections


let tests = [ 
  "test all_intersections" >:: (fun _ -> assert_equal
    ([])
    (all_intersections gamestate.curr_player.p_roads));

]



let _ = run_test_tt_main ("suite" >::: tests)
