open Note
open Brr
open Brr_note

let v = Jstr.v

let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some root ->
    let incr_button = El.(button [txt' "click me"]) in

    let incr x = x + 1 in
    let incr_e = Evr.on_el Ev.click (fun _ -> incr) incr_button in
    let counter_s = S.accum 0 incr_e in
    let message_s = S.map (
      fun count ->
        [El.txt' (Printf.sprintf "you clicked %d times" count)]
    ) counter_s in

    let p = El.p [] in
    Elr.def_children p message_s;

    let d = El.div [p; incr_button] in
    El.set_children root [d]

let () = main "root"
