open Printf

let rec replicate x = function
  | 0 -> []
  | n -> x :: replicate x (n-1)

let rec seq = function
  | 0 -> []
  | n -> n :: seq (n-1)

let print_table to_s header rows =
  let widths = List.map String.length header in
  let print_row r =
    List.iter2 (fun w c -> printf "%*s" w (to_s c)) widths r;
    Printf.printf "\n"
  in
  List.iter (fun h -> printf "%s" h) header;
  printf "\n";
  List.iter print_row rows

let time f x =
  let t0 = Sys.time () in
  let fx = f x in
  let t1 = Sys.time () in
  Printf.printf "Execution time: %fs\n" (t1 -. t0);
  fx
