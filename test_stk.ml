let main () =
  let st = Stk.create () in
  let rec loop () =
    match Util.read_line () with
      | None -> ()
      | Some line -> 
        begin
          let args = Array.of_list (Util.split line) in
          match args.(0) with
            | "push" -> Stk.push st (int_of_string args.(1)); loop ()
            | "pop" -> 
              begin
                try 
                  print_endline (string_of_int (Stk.pop st)) ; loop ()
                with Stk.Stack_empty -> loop ()
              end
            | cmd -> ignore(failwith ("invalid argument " ^ cmd))
        end
  in
  loop ()

let _ = main ()             
