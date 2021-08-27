module Http = struct
  open Js_of_ocaml

  (* request are in the payload of body of a HTTP POST; replies are in
     the body of a POST's response *)
  let do_post ~url ~request send =
    let req = XmlHttpRequest.create() in
    req##_open (Js.string "post") (Js.string url) Js._true;

    let callback _ =
      match req##.readyState with
      | XmlHttpRequest.DONE -> (
        match Js.Opt.to_option req##.responseText with
        | None -> ()
        | Some s -> send (Js.to_string s)
      )
      | _ -> ()
    in
    req##.onreadystatechange := Js.wrap_callback callback;
    req##send (Js.some (Js.string request))

  open Note

  let req url request =
    let response, send = E.create () in
    do_post ~url ~request send;
    response

end

open Note

module P = Proto.Protocol_j

module Protocol = struct

  let url = "http://localhost:8888/"

  let req : P.front_to_back -> P.back_to_front event =
    fun request ->
    let request_p = P.string_of_front_to_back request in
    let response_p = Http.req url request_p in
    E.map P.back_to_front_of_string response_p

end

module Adder : sig
  val create : (int * int) event -> int event
end =
struct
  type state = {
    session : P.session option;
    adds : (int * int) list
  }

  let add_add add adds = List.rev (add :: List.rev adds)
  let rem_add adds = match adds with [] -> [] | _ :: adds -> adds

  let do_add_add : state -> int * int -> state =
    fun s add -> { s with adds = add_add add s.adds}

  let do_req : state -> state * P.back_to_front event =
    fun s -> match s.session with
      | None -> s, (Protocol.req `NewSession)
      | Some _ when s.adds = [] -> s, E.never
      | Some session -> s, Protocol.req (`Add (session, List.hd s.adds))

  let do_resp : state -> P.back_to_front -> state * int option =
    fun s resp -> match resp with
      | `Add i -> { s with adds = rem_add s.adds}, Some i
      | `NewSession session -> { s with session = Some session }, None
      | `InvalidSession -> { s with session = None }, None

  let create add_ev =
    let init = { session = None; adds = [] } in
    let def s =
      let do_req = E.map do_req (S.changes s) in
      let resp = E.join (E.Pair.snd do_req) in
      let do_resp = S.sample s ~on:resp do_resp in
      let result = E.Option.on_some @@ E.Pair.snd do_resp in
      let s'0 = E.Pair.fst do_req in
      let s'1 = E.Pair.fst do_resp in
      let s'2 = S.sample s ~on:add_ev do_add_add in
      let s' = S.hold init (E.select [s'0; s'1; s'2]) in
      s', (s', result)
    in
    let s, result = S.fix init def in
    Logr.hold (S.log s (fun _ -> ()));
    result
end

open Brr
open Brr_note

let v = Jstr.v

let main root =
  let arg1 = El.input () in
  let arg2 = El.input () in
  let sum_el = El.span [] in
  let sum_button = El.(button [txt' "sum"]) in
  let table = El.table [
    El.tr [El.(td [txt' "arg 1:"; arg1 ])];
    El.tr [El.(td [txt' "arg 2:"; arg2 ])];
    El.tr [El.td [sum_button; sum_el]];
  ] in
  El.set_children root [table];

  let int_of_value el =
    int_of_string (Jstr.to_string (El.prop El.Prop.value el))
  in

  let get_args _ =
    try
      let arg1_i = int_of_value arg1 in
      let arg2_i = int_of_value arg2 in
      Some (arg1_i, arg2_i)
    with Failure _ ->
      None
  in

  let add_opt_ev = Evr.on_el Ev.click get_args sum_button in
  let add_ev = E.Option.on_some add_opt_ev in

  let sum_e = Adder.create add_ev in
  let sum_txt = E.map (fun i -> [El.txt' (string_of_int i)]) sum_e in
  Elr.set_children sum_el ~on:sum_txt


let () =
  let id = "root" in
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(info [str (Printf.sprintf "element %S not found" id)])
  | Some root ->
    main root
