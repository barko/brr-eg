open Brr

let v = Jstr.v

let main id () =
  match El.find_id (v id) with
  | None -> Debug.pr "element %S not found\n" id
  | Some el ->
    let email = El.input ~atts:[Att.placeholder (v "email")] [] in

    let password =
      let atts = [v "type", v "password"; Att.placeholder (v "password")] in
      El.input ~atts []
    in

    let submit_button =
      let atts = [v "type", v "submit"] in
      El.input ~atts []
    in

    let table = El.table [
      El.tr [El.td [email]];
      El.tr [El.td [password]];
      El.tr [El.td [submit_button]]

    ] in

    El.set_children el [table]

let () = App.run (main "root")
