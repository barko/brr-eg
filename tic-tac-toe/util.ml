(* utilities *)
module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let range =
  fun n ->
  let open Seq in
  let rec gen x () =
    if x = n then
      Nil
    else
      Cons (x, gen (x + 1))
  in
  gen 0

let get map i =
  IntMap.find i map

let sp = Printf.sprintf
let v = Brr.Jstr.v

(* remove the first n elements of the list *)
let decapitate_list =
  let rec loop n i = function
    | [] -> []
    | (_ :: t) as list ->
      if i = n then
        list
      else
        loop n (i + 1) t
  in
  fun n list ->
    loop n 0 list

