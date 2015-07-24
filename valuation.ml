type 'a t = 'a -> bool

let empty = fun _ -> false

let lookup x env = env x

let lookupd x env =
  try Some (lookup x env) with Not_found -> None

let extend x v env =
  fun y -> if x = y then v else env y

let all =
  let (>>=) m f = List.flatten (List.map f m) in
  let rec all_aux = function
    | [] -> [empty]
    | x::xs ->
       [x, false; x, true] >>= fun (x, v) ->
       all_aux xs >>= fun env ->
       [extend x v env]
  in all_aux

let on_all xs f =
  let rec on_all_aux env = function
    | [] -> f env
    | x::xs ->
       on_all_aux (extend x false env) xs
       && on_all_aux (extend x true env) xs
  in
  on_all_aux empty xs
