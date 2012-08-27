module type UF =
sig
  type t
  val init: int -> t
  val union: t -> int -> int -> unit
  val connected : t -> int -> int -> bool
  val print : t -> unit
end

module QuickfindUF : UF =
struct
  
  type t = int array
  
  let init n = 
    let a = Array.make n 0 in
    for i = 0 to (n - 1) do a.(i) <- i done;
    a

  let union a i j =
    let i_val = a.(i) in
    let j_val = a.(j) in
    for i = 0 to (Array.length a - 1)
    do
      if a.(i) = i_val then a.(i) <- j_val
    done

  let connected a i j = (a.(i) = a.(j))

  let print a =
    (Array.iter (fun i -> Printf.printf "%d " i) a; print_newline ())

end

module QuickUnionUF : UF =
struct

  type t = int array

  let init n = 
    let a = Array.make n 0 in
    for i = 0 to (n - 1) do a.(i) <- i done;
    a

  let rec root a i = 
    if i = a.(i) then i else root a a.(i)

  let connected a i j =
    let i_root = root a i in
    let j_root = root a j in
    i_root = j_root

  let union a i j = 
    let i_root = root a i in
    let j_root = root a j in
    (* Make j's root the parent of i's root *)
    a.(i_root) <- j_root

  let print a =
    (Array.iter (fun i -> Printf.printf "%d " i) a; print_newline ())

end

module WeightedUnion : UF =
struct
  type t = {a : int array; sz : int array}
      
  let init n = 
    let a = Array.make n 0 in
    let sz = Array.make n 1 in
    for i = 0 to (n - 1) do a.(i) <- i done;
    {a = a; sz = sz}

  let rec root a i = 
    if a.a.(i) = i then i else root a a.a.(i)

  let connected a i j =
    let i_root = root a i in
    let j_root = root a j in
    i_root = j_root

  let union a i j = 
    let i_root = root a i in
    let j_root = root a j in
    let i_sz = a.sz.(i) in
    let j_sz = a.sz.(j) in
    
    if i_sz < j_sz 
    then
      begin
        a.a.(i_root) <- j_root;
        a.sz.(j) <- j_sz + i_sz
      end
    else
      begin
        a.a.(j_root) <- i_root;
        a.sz.(i) <- i_sz + j_sz
      end

  let print a =
    (Array.iter (fun i -> Printf.printf "%d " i) a.a; print_newline ())
      
end

module WeightedPathCompressedUF1 : UF = 
struct

  type t = {a : int array; sz : int array}
      
  let init n = 
    let a = Array.make n 0 in
    let sz = Array.make n 1 in
    for i = 0 to (n - 1) do a.(i) <- i done;
    {a = a; sz = sz}

  let root a i = 
    let rec root_aux i = 
      if a.a.(i) = i then i else root_aux a.a.(i) in
    let rec set_root i root = 
      if i = root  then () 
      else 
        let p = a.a.(i) in
        (a.a.(i) <- root; set_root p root) in
    let r = root_aux i in
    (set_root i r; r)

  let connected a i j =
    let i_root = root a i in
    let j_root = root a j in
    i_root = j_root

  let union a i j = 
    let i_root = root a i in
    let j_root = root a j in
    let i_sz = a.sz.(i) in
    let j_sz = a.sz.(j) in
    
    if i_sz < j_sz 
    then
      begin
        a.a.(i_root) <- j_root;
        a.sz.(j) <- j_sz + i_sz
      end
    else
      begin
        a.a.(j_root) <- i_root;
        a.sz.(i) <- i_sz + j_sz
      end
  
  let print a =
    (Array.iter (fun i -> Printf.printf "%d " i) a.a; print_newline ())
      
end

module U = WeightedPathCompressedUF1

let main () =
  
  let n = int_of_string (input_line stdin) in
  let a = U.init n in
  let rec get_line () =
    try 
      let line = String.trim (input_line stdin) in
      if line = "" then get_line () 
      else if Util.startswith line "#" then get_line ()
      else Some line
    with End_of_file -> None 
  in 
  let loop = ref true in
  while !loop do
    match get_line () with
      | None -> (loop := false; U.print a)
      | Some line ->
        let words = Util.split line in
        let cmd = List.hd words in
        let i = int_of_string (List.nth words 1) in
        let j = int_of_string (List.nth words 2) in
        match cmd with
          | "union" ->
            begin
              U.union a i j;
              print_string line;
              print_newline ()
            end
          | "connected" -> 
            begin
              print_string (line ^ " " ^ string_of_bool (U.connected a i j));
              print_newline ()
            end
          | _ -> failwith ("unknown command: " ^ cmd)
  done
    
