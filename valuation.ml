type 'a t = 'a -> bool

let empty = fun _ -> false

let extend x v env = fun y -> if x = y then v else env y

let lookup x env = env x

let lookupd x env =
  try Some (lookup x env) with Not_found -> None

let rec all ks =
  let (>>=) m f = List.flatten (List.map f m) in
  match ks with
  | [] -> [empty]
  | k::ks' ->
    [k,false; k,true] >>= fun (k,v) ->
    all ks' >>= fun env ->
    [extend k v env]
