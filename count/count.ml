open Note
open Brr

let v = Jstr.v

let main id () =
  match El.find_id (v id) with
  | None -> Debug.pr "element %S not found\n" id
  | Some root ->
    let incr_button = El.button [`Txt (v "click me")] in

    let incr x = x + 1 in
    let incr_e = Ev.(for_el incr_button click (fun _ -> incr)) in
    let counter_s = S.accum 0 incr_e in
    let message_s = S.map (
      fun count ->
        [`Txt (v (Printf.sprintf "you clicked %d times" count))]
    ) counter_s in

    let p = El.p [] in
    El.def_children p message_s;

    let d = El.div [p; incr_button] in
    El.set_children root [d]

let () = App.run (main "root")
