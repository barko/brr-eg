open Util

type player = X | O

type field =
  | Empty
  | Marked of player

type turn =
  | Playing of player
  | Winner of player
  | Draw

type state = {
  board : field IntMap.t;
  turn : turn;
}

open Note
open Brr

type msg = [ `JumpTo of int | `Mark of int ]

type snapshot = {
  state : state;
  button : El.t;
  jump : msg event;
}

type t = {
  history : snapshot list;
  current : int; (* index into history list *)
}

let empty_state =
  let empty_board = Seq.fold_left (
    fun board i ->
      IntMap.add i Empty board
  ) IntMap.empty (range 9) in
  { board = empty_board; turn = Playing X }

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

open Brr_note

let jump_button idx =
  let txt =
    if idx = 0 then
      [El.txt' "new game"]
    else
      [El.txt' (sp "go to move #%d" idx)]
  in
  let button = El.button txt in
  let ev = Evr.on_el Ev.click (fun _ -> `JumpTo idx) button in
  El.li [button], ev

let empty =
  let button, jump = jump_button 0 in
  let snapshot_0 = { state = empty_state; button; jump } in
  { history = [snapshot_0]; current = 0 }

let at1 c =
  [At.class' (v c)]

let txt_of_turn gs =
  [El.txt' (string_of_turn gs)]

let cell idx =
  let el = El.button ~at:(at1 "square") [] in
  let ev = Evr.on_el Ev.click (fun _ -> idx) el in
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

let update t msg =
  match msg with
  | `JumpTo move -> { t with current = move }
  | `Mark idx ->
    let depth = List.length t.history in
    let snap , history_rest =
      if depth = t.current + 1 then
        match t.history with
        | hd :: tl -> hd, tl
        | [] -> assert false
      else
        match decapitate_list (depth - t.current - 1) t.history with
        | hd :: tl -> hd, tl
        | [] -> assert false
    in
    match get snap.state.board idx with
    | Empty -> (
        match snap.state.turn with
        | Playing player ->
          let board = IntMap.add idx (Marked player) snap.state.board in
          let turn =
            if is_draw board then
              Draw
            else
              match winner board with
              | None -> Playing (next_player player)
              | Some winner -> Winner winner
          in
          let state' = { turn; board } in
          let current = List.length history_rest + 1 in
          let button, jump = jump_button current in
          let snap' = { state = state'; button; jump } in
          let history = snap' :: snap :: history_rest in
          { history; current }
        | Draw
        | Winner _ -> t
      )
    | Marked _ -> t


let buttons_jumps t =
  let events, jumps = List.fold_left (
    fun (buttons, jumps) { button; jump; _ } ->
      button :: buttons, jump :: jumps
  ) ([], []) t.history in
  events, E.select jumps


let cell_txt = function
  | Empty    -> [           ]
  | Marked X -> [El.txt' "X"]
  | Marked O -> [El.txt' "O"]

let ui () =
  let sq, cell_ev, cells = square () in
  let turn_div = El.div [] in
  let game_board = El.div ~at:(at1 "game-board") [sq] in
  let jump_button_list = El.ol [] in
  let game_info = El.div ~at:(at1 "game-info")
      [turn_div; jump_button_list] in
  let game = El.div ~at:(at1 "game") [game_board; game_info] in
  let eq = ( == ) in

  let def t_s =
    let buttons_jumps_s = S.map ~eq buttons_jumps t_s in
    let buttons_s = S.Pair.fst ~eq buttons_jumps_s in
    let jumps_s = S.Pair.snd ~eq buttons_jumps_s in
    let jump_e = E.swap jumps_s in

    let ev = E.select [cell_ev; jump_e] in
    Elr.def_children jump_button_list buttons_s;

    let state_s = S.map ~eq (
      fun { history; current } ->
        let hlen = List.length history in
        let { state; _ } = List.nth history (hlen - current - 1) in
        state
    ) t_s in

    let board_s = S.map ~eq (fun { board; _ } -> board) state_s in
    let turn_s = S.map ~eq (fun { turn; _} -> turn) state_s in

    let cell_s i = S.map ~eq (fun board -> get board i) board_s in
    let cell_s_seq = Seq.map cell_s (range 9) in
    let cell_s_arr = Array.of_seq cell_s_seq in
    Array.iteri (
      fun i cell_s ->
        let cell_txt_s = S.map ~eq cell_txt cell_s in
        Elr.def_children cells.(i) cell_txt_s
    ) cell_s_arr;

    let turn_txt_s = S.map ~eq txt_of_turn turn_s in
    Elr.def_children turn_div turn_txt_s;

    let t_ev = S.sample t_s ~on:ev update in
    let t_s' = S.hold ~eq empty t_ev in
    t_s', t_s'
  in
  S.fix ~eq empty def, game

let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some root ->
    let t_s, game = ui () in
    Logr.hold (S.log t_s (fun _ -> ()));
    El.set_children root [game]


let () = main "root"
