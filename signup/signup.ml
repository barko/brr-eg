(* allow user to choose between logging in and resetting password *)

open Brr
open Brr_note

let v = Jstr.v

let login =
  let email = El.input ~at:[At.placeholder (v "email")] () in

  let password =
    let at = [At.type' (v "password"); At.placeholder (v "password")] in
    El.input ~at ()
  in

  let submit_button =
    let at = [At.type' (v "submit"); At.value (v "login")] in
    El.input ~at ()
  in

  let reset_link = El.a ~at:[At.href (v "#")] [El.txt' "forgot password?"] in
  let click_reset = Evr.on_el Ev.click (fun _ -> `Reset) reset_link in

  let table = El.table [
    El.tr [El.td [email]];
    El.tr [El.td [password]];
    El.tr [El.td [submit_button; reset_link]]

  ] in
  table, click_reset

let reset =
  let email = El.input ~at:[At.placeholder (v "email")] () in

  let submit_button =
    let at = [At.type' (v "submit"); At.value (v "reset")] in
    El.input ~at ()
  in

  let login_link = El.a ~at:[At.href (v "#")] [El.txt' "login"] in
  let click_login = Evr.on_el Ev.click (fun _ -> `Login) login_link in

  let table = El.table [
    El.tr [El.td [email]];
    El.tr [El.td [submit_button; login_link]]

  ] in
  table, click_login


let main id =
  match Document.find_el_by_id G.document (v id) with
  | None -> Console.(debug [str (Printf.sprintf "element %S not found" id)])
  | Some el ->
    let login_el, click_reset = login in
    let reset_el, click_login = reset in

    let open Note in
    let reset_or_login_s = S.hold `Login (E.select [click_reset; click_login]) in
    let children_s = S.map (
      function
      | `Reset -> [reset_el]
      | `Login -> [login_el]
    ) reset_or_login_s in

    Elr.def_children el children_s


let () = main "root"
