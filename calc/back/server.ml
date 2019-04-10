open Cohttp
open Cohttp_lwt_unix

module P = Proto.Protocol_j
module SS = Set.Make(String)
type state = SS.t

(* deal with CORS, if we have to *)
let add_access_control request response_headers =
  let request_headers = Request.headers request in
  match Header.get request_headers "Origin" with
  | Some origin ->
    let h = [
      (* allow the origin, in lieu of responding with just
         wildcard "*" *)
      "Access-Control-Allow-Origin", origin;

      (* also allow credentials, in case request was made with
         "withCredentials = true" *)
      "Access-Control-Allow-Credentials", "true"
    ] in

    Header.add_list response_headers h

  | _ -> response_headers

let server () =
  let mode = `TCP (`Port 8888) in
  let state_0 = SS.empty in
  let state_mvar = Lwt_mvar.create state_0 in

  let callback _conn req body =
    let headers = Header.init () in
    let not_found = `Not_found, "", headers in

    let%lwt (status, body, headers) =
      match Request.meth req with
      | `POST -> (
          let%lwt body_s = Cohttp_lwt.Body.to_string body in
          try
            let f2b = P.front_to_back_of_string body_s in
            let%lwt state = Lwt_mvar.take state_mvar in
            let state, response =
              match f2b with
              | `NewSession ->
                let session = Int64.to_string (Random.int64 Int64.max_int) in
                let state = SS.add session state in
                state, `NewSession session

              | `Add (session, (a, b)) ->
                match SS.find_opt session state with
                | None -> state, `InvalidSession
                | Some _ -> state, `Add (a + b)
            in
            let%lwt () = Lwt_mvar.put state_mvar state in
            let b2f = P.string_of_back_to_front response in
            let headers = add_access_control req headers in
            Lwt.return (`OK, b2f, headers)

          with
          | Yojson.Json_error err_msg
          | Atdgen_runtime__Oj_run.Error err_msg ->
            let response_headers = Header.add headers
                "content-type" "text/plain" in
            let body = "Error: " ^ err_msg in
            Lwt.return (`OK, body, response_headers)
        )

      | _ ->
        Lwt.return not_found
    in

    Server.respond_string ~headers ~status ~body ()
  in

  Server.create ~mode (Server.make ~callback ())

let _ =
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  Lwt.async_exception_hook := ignore;
  ignore (Lwt_main.run (server ()))

