let rec replicate x = function
  | 0 -> []
  | n -> x :: replicate x (n-1)

let rec map2_ex def f l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (x::l1',[])-> f x def :: map2_ex def f l1' []
  | ([],y::l2') -> f y def :: map2_ex def f [] l2'
  | (x::l1',y::l2') -> f x y :: map2_ex def f l1' l2'

let time f x =
  let t0 = Sys.time () in
  let fx = f x in
  let t1 = Sys.time () in
  Printf.printf "Execution time: %fs\n" (t1 -. t0);
  fx

let rec seq = function
  | 0 -> []
  | n -> n :: seq (n-1)
