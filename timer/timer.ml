open Note
open Brr
open Brr_note
open Brr_note_kit

let v = Jstr.v

(* [every secs] creates a unit event which fires every [secs] seconds *)
let every secs =
  let e, send_e = E.create () in
  let rec loop () =
    Time.delay secs (fun () -> loop ());
    send_e ()
  in
  loop ();
  e


let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some root ->

    let delta_secs = 1.0 in
    let add x y = x +. y in
    let e = every delta_secs in
    let incr_e = E.map (fun () -> add delta_secs) e in
    let sum_e = E.accum 0.0 incr_e in
    let sum_s = S.hold 0.0 sum_e in

    let sum_txt f = [El.txt' (Printf.sprintf "%0.1f" f)] in
    let sum_txt_s = S.map sum_txt sum_s in
    Elr.def_children root sum_txt_s

let () = main "root"
