open Note
open Brr

module Counter : sig
  type t
  val create : unit -> t
  val incr : t -> t
  val decr : t -> t
  val value : t -> int
end = struct
  type t = int
  let create () = 0
  let incr = succ
  let decr = pred
  let value x = x
end

module Counters : sig
  type t

  type idx = int
  val empty : t
  val count : t -> int
  val add : Counter.t -> t -> t
  val update : int -> by:(Counter.t -> Counter.t option) -> t -> t
  val foldi : (int -> Counter.t -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  type t = Counter.t list
  type idx = int
  let empty = []
  let count = List.length
  let add c cs = c :: cs
  let update i ~by cs =
    let rec loop k left = function
    | [] -> invalid_arg (Printf.sprintf "No counter identified by %d" i)
    | c :: cs when i <> k -> loop (k + 1) (c :: left) cs
    | c :: cs ->
        match by c with
        | None -> List.rev_append left cs
        | Some c -> List.rev_append (c :: left) cs
    in
    loop 0 [] cs

  let foldi f cs acc =
    let rec loop f i acc = function
    | [] -> acc
    | c :: cs -> loop f (i + 1) (f i c acc) cs
    in
    loop f 0 acc cs
end

type counter_action = [
  | `Incr
  | `Decr
  | `Delete
]

let txt s =
  [`Txt (Jstr.v s)]

let counter_ui :
  label:Jstr.t -> Counter.t -> counter_action event * [> El.t] =
fun ~label counter ->
  let label = El.span [`Txt label] in
  let decr_button = El.button (txt "-") in
  let incr_button = El.button (txt "+") in
  let delete_button = El.button (txt "x") in
  let value = El.span (txt (string_of_int (Counter.value counter))) in
  let el = El.div [label; decr_button; value; incr_button; delete_button] in
  let decr = Ev.(for_el decr_button click (stamp `Decr)) in
  let incr = Ev.(for_el incr_button click (stamp `Incr)) in
  let delete = Ev.(for_el delete_button click (stamp `Delete)) in
  let action = E.select [decr; incr; delete] in
  action, el

type counters_action = [
  | `Add
  | `Update of Counters.idx * counter_action
]

let counters_ui : Counters.t -> counters_action event * [> El.t] =
fun cs ->
  let cs_count = Counters.count cs in
  let counter_ui i c (actions, els) =
    let action, el = counter_ui ~label:(Jstr.of_int (cs_count - i)) c in
    let action = E.map (fun act -> i, act) action (* remember index *) in
    (action :: actions, el :: els)
  in
  let actions, els = Counters.foldi counter_ui cs ([], []) in
  let update = E.map (fun act -> `Update act) (E.select actions)  in
  let add_button = El.button (txt "Add counter") in
  let add = Ev.(for_el add_button click (stamp `Add)) in
  E.select [add; update], El.div [add_button; El.div (List.rev els)]

let update_counters : counters_action -> Counters.t -> Counters.t =
fun action cs -> match action with
| `Add -> Counters.add (Counter.create ()) cs
| `Update (idx, action) ->
    let action c = match action with
    | `Incr -> Some (Counter.incr c)
    | `Decr -> Some (Counter.decr c)
    | `Delete -> None
    in
    Counters.update idx ~by:action cs

let ui cs =
  let def cs =
    let counters_ui = S.map ~eq:( == ) counters_ui cs in
    let action = S.Pair.fst ~eq:( == ) counters_ui in
    let el     = S.Pair.snd ~eq:( == ) counters_ui in
    let update = E.swap action in
    let do_action = E.map update_counters update in
    let cs' = S.accum (S.value cs) do_action in
    cs', (cs', el)
  in
  S.fix cs def

let main id () =
  match El.find_id (Jstr.v id) with
  | None -> Debug.pr "element %S not found\n" id
  | Some el ->
    let cs, ui_el = ui Counters.empty in
    Logr.hold (S.log cs (fun _ -> ()));
    El.def_children el (S.map (fun el -> [el]) ui_el)

let () =
  App.run ~name:"counters" (main "root")
