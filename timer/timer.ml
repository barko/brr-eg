open Note
open Brr

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


let main id () =
  match El.find_id (v id) with
  | None -> Debug.pr "element %s not found\n" id;
  | Some root ->

    let delta_secs = 1.0 in
    let add x y = x +. y in
    let e = every delta_secs in
    let incr_e = E.map (fun () -> add delta_secs) e in
    let sum_e = E.accum 0.0 incr_e in
    let sum_s = S.hold 0.0 sum_e in

    let sum_txt f = [`Txt (v (Printf.sprintf "%0.1f" f))] in
    let sum_txt_s = S.map sum_txt sum_s in
    El.def_children root sum_txt_s

let () = App.run (main "root")
