let rec all_valuations ks vs =
  let (>>=) m f = List.flatten (List.map f m) in
  match ks with
  | [] -> [[]]
  | k::ks' ->
     List.map (fun v -> (k,v)) vs >>= fun kv ->
     all_valuations ks' vs >>= fun vl ->
     [kv :: vl]

let time f x =
    let t0 = Sys.time () in
    let fx = f x in
    let t1 = Sys.time () in
    Printf.printf "Execution time: %fs\n" (t1 -. t0);
    fx

let rec seq = function
  | 0 -> []
  | n -> n :: seq (n-1)
