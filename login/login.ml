open Brr

let v = Jstr.v

let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some el ->
    let email = El.input ~at:[At.placeholder (v "email")] () in

    let password =
      let at = [At.type' (v "password"); At.placeholder (v "password")] in
      El.input ~at ()
    in

    let submit_button =
      let at = [At.type' (v "submit")] in
      El.input ~at ()
    in

    let table = El.table [
      El.tr [El.td [email]];
      El.tr [El.td [password]];
      El.tr [El.td [submit_button]]

    ] in

    El.set_children el [table]

let () = main "root"
