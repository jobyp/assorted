let main () =
  let q = Que.create () in
  let rec loop () =
    match (Util.read_line ()) with
      | None -> ()
      | Some line -> 
        let lst = Util.split line in
        match (List.hd lst) with
          | "enque" -> Que.enque q (int_of_string (List.nth lst 1)); loop ()
          | "deque" -> print_endline (string_of_int (Que.deque q)); loop ()
          | _ -> failwith "invalid argument" in
  loop ()

let _ = main ()
