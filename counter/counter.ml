open Note
open Brr

let str = Jstr.v

let main id () =
  match El.find_id (str id) with
  | None -> Debug.pr "element %S not found\n" id
  | Some el ->
    let decr_button = El.button [`Txt (str "-")] in
    let incr_button = El.button [`Txt (str "+")] in

    let incr x = x + 1 in
    let decr x = x - 1 in

    let decr_e = Ev.(for_el decr_button click (fun _ -> decr)) in
    let incr_e = Ev.(for_el incr_button click (fun _ -> incr)) in

    let decr_incr_e = E.select [decr_e; incr_e] in
    let counter_s = S.accum 0 decr_incr_e in

    let children_s = S.map (
      fun count -> [
          decr_button;
          `Txt (str (string_of_int count));
          incr_button
        ]
    ) counter_s in

    El.def_children el children_s

let () = App.run (main "root")
