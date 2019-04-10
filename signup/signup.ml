(* allow user to choose between logging in and resetting password *)

open Note
open Brr

let v = Jstr.v

let login =
  let email = El.input ~atts:[Att.placeholder (v "email")] [] in

  let password =
    let atts = [v "type", v "password"; Att.placeholder (v "password")] in
    El.input ~atts []
  in

  let submit_button =
    let atts = [v "type", v "submit"; v "value", v "login"] in
    El.input ~atts []
  in

  let reset_link = El.a ~atts:[Att.href (v "#")] [`Txt (v "forgot password?")] in
  let click_reset = Ev.(for_el reset_link click (fun _ -> `Reset)) in

  let table = El.table [
    El.tr [El.td [email]];
    El.tr [El.td [password]];
    El.tr [El.td [submit_button; reset_link]]

  ] in
  table, click_reset

let reset =
  let email = El.input ~atts:[Att.placeholder (v "email")] [] in

  let submit_button =
    let atts = [v "type", v "submit"; v "value", v "reset"] in
    El.input ~atts []
  in

  let login_link = El.a ~atts:[Att.href (v "#")] [`Txt (v "login")] in
  let click_login = Ev.(for_el login_link click (fun _ -> `Login)) in

  let table = El.table [
    El.tr [El.td [email]];
    El.tr [El.td [submit_button; login_link]]

  ] in
  table, click_login


let main id () =
  match El.find_id (v id) with
  | None -> Debug.pr "element %S not found\n" id
  | Some el ->
    let login_el, click_reset = login in
    let reset_el, click_login = reset in

    let reset_or_login_s = S.hold `Login (E.select [click_reset; click_login]) in
    let children_s = S.map (
      function
      | `Reset -> [reset_el]
      | `Login -> [login_el]
    ) reset_or_login_s in

    El.def_children el children_s


let () = App.run (main "root")
