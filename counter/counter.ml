open Note
open Brr
open Brr_note

let v = Jstr.v

let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some el ->
    let decr_button = El.(button [txt' "-"]) in
    let incr_button = El.(button [txt' "+"]) in

    let incr x = x + 1 in
    let decr x = x - 1 in

    let decr_e = Evr.on_el Ev.click (fun _ -> decr) decr_button in
    let incr_e = Evr.on_el Ev.click (fun _ -> incr) incr_button in

    let decr_incr_e = E.select [decr_e; incr_e] in
    let counter_s = S.accum 0 decr_incr_e in

    let children_s = S.map (
      fun count -> [
          decr_button;
          El.txt' (string_of_int count);
          incr_button
        ]
    ) counter_s in

    Elr.def_children el children_s

let () = main "root"
