let rec replicate x = function
  | 0 -> []
  | n -> x :: replicate x (n-1)

let rec seq = function
  | 0 -> []
  | n -> n :: seq (n-1)

let print_table cell_width to_s header rows =
  let open Printf in
  let print_row r =
    let cs = List.map (fun c -> sprintf "%*s" cell_width (to_s c)) r in
    String.concat " | " cs |> printf "| %s |\n"
  in
  let hds = List.map (fun c -> sprintf "%*s" cell_width c) header in
  let divs = List.map (fun _ -> String.make cell_width '-') header in
  String.concat " | " hds |> printf "| %s |\n";
  String.concat "-+-" divs |> printf "+-%s-+\n";
  List.iter print_row rows

let time f x =
  let t0 = Sys.time () in
  let fx = f x in
  let t1 = Sys.time () in
  Printf.printf "Execution time: %fs\n" (t1 -. t0);
  fx
