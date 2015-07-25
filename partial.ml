type ('a, 'b) t = ('a * 'b) list

let undefined = []

let is_undefined f = f = []

let singleton x y = [x, y]

let defined = List.mem_assoc

let undefine x =
  let rec undefine_aux acc = function
    | [] -> acc
    | (y,v)::tl when x <> y -> undefine_aux ((y,v)::acc) tl
    | _::tl -> undefine_aux acc tl
  in
  undefine_aux []

let update x y f = (x, y)::(undefine x f)

let apply f x = List.assoc x f

let applyd f x ~default =
  try apply f x with Not_found -> default

let map g =
  List.map (fun (x, v) -> (x, g v))

module Infix =
  struct
    let (|=>) = singleton
    let (|->) = update
  end
