open Util

type player = X | O

type field =
  | Empty
  | Marked of player

type turn =
  | Playing of player
  | Winner of player
  | Draw

type t = {
  board : field IntMap.t;
  turn : turn;
}

let empty = {
  board = Seq.fold_left (fun board i -> IntMap.add i Empty board) IntMap.empty (range 9);
  turn = Playing X;
}

let winning_combinations = [
  0, 1, 2;
  3, 4, 5;
  6, 7, 8;
  0, 3, 6;
  1, 4, 7;
  2, 5, 8;
  0, 4, 8;
  2, 4, 6;
]

let winner board =
  try
    let i, _j, _k = List.find (
      fun (i, j, k) ->
        match get board i, get board j, get board k with
        | Marked X, Marked X, Marked X
        | Marked O, Marked O, Marked O -> true
        | _ -> false
    ) winning_combinations in
    match get board i with
    | Marked player -> Some player
    | _ -> assert false
  with Not_found ->
    None

let is_draw board =
  IntMap.fold (
    fun _ field all ->
      all && (match field with Marked _ -> true | Empty -> false)
  ) board true


let next_player = function
  | X -> O
  | O -> X

let string_of_turn = function
  | Playing X -> "next player: X"
  | Playing O -> "next player: O"
  | Winner X  -> "winner: X"
  | Winner O  -> "winner: O"
  | Draw      -> "draw"

open Note
open Brr

let at1 c =
  [At.class' (v c)]

let txt_of_turn gs =
  [`Txt (v (string_of_turn gs))]

let cell idx =
  let el = El.button ~at:(at1 "square") [] in
  let ev = Ev.(for_el el click (fun _ -> idx)) in
  el, ev

let row start_idx =
  let b0, e0 = cell (start_idx    ) in
  let b1, e1 = cell (start_idx + 1) in
  let b2, e2 = cell (start_idx + 2) in
  let el = El.div ~at:(at1 "board-row") [b0; b1; b2] in
  el, b0, b1, b2, E.select [e0; e1; e2]

let square () =
  let r0, b0, b1, b2, ev012 = row 0 in
  let r1, b3, b4, b5, ev345 = row 3 in
  let r2, b6, b7, b8, ev678 = row 6 in
  let el = El.div ~at:(at1 "game-board") [r0; r1; r2] in
  let ev = E.select [ev012; ev345; ev678] in
  let ev_w = E.map (fun idx -> `Mark idx) ev in
  let cells = [| b0; b1; b2; b3; b4; b5; b6; b7; b8 |] in
  el, ev_w, cells

let update msg t =
  match msg with
  | `NewGame -> empty
  | `Mark idx ->
    match get t.board idx with
    | Empty -> (
        match t.turn with
        | Playing player ->
          let board = IntMap.add idx (Marked player) t.board in
          let turn =
            if is_draw board then
              Draw
            else
              match winner board with
              | None -> Playing (next_player player)
              | Some winner -> Winner winner
          in
          { turn; board }
        | Draw
        | Winner _ -> t
      )
    | Marked _ -> t

let cell_txt = function
  | Empty    -> [            ]
  | Marked X -> [`Txt (v "X")]
  | Marked O -> [`Txt (v "O")]

let ui () =
  let new_game = El.button [`Txt (v "new game")] in
  let new_game_ev = Ev.(for_el new_game click (fun _ -> `NewGame)) in
  let sq, ev_idx, cells = square () in
  let ev = E.select [new_game_ev; ev_idx] in

  let update_idx = E.map update ev in
  let t_ev = E.accum empty update_idx in
  let t_s = S.hold empty t_ev in
  let board_s = S.map (fun { board; _ } -> board) t_s in
  let turn_s = S.map (fun { turn; _} -> turn) t_s in
  let cell_s i = S.map (fun board -> get board i) board_s in
  let cell_s_seq = Seq.map cell_s (range 9) in
  let cell_s_arr = Array.of_seq cell_s_seq in
  Array.iteri (
    fun i cell_s ->
      let cell_txt_s = S.map cell_txt cell_s in
      El.def_children cells.(i) cell_txt_s
  ) cell_s_arr;

  let turn_txt_s = S.map txt_of_turn turn_s in
  let game_outcome = El.div [] in
  El.def_children game_outcome turn_txt_s;

  let game_board = El.div ~at:(at1 "game-board") [sq] in
  let game_info = El.div ~at:(at1 "game-info") [game_outcome; new_game] in
  El.div ~at:(at1 "game") [game_board; game_info]

let main id () =
  match El.find_id (v id) with
  | None -> Debug.pr "element %S not found" id
  | Some el ->
    let game = ui () in
    El.set_children el [game]

let () = App.run (main "root")
