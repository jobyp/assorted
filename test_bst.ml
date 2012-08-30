let random_list () =
  let a = Array.of_list (Util.range 10) in
    (Jsort.shuffle a; Array.to_list a)

let main () =
  let lst = random_list () in
  let bst = Bst.from_list lst in
  let lst_sorted = Bst.to_list bst in
    begin
      List.iter (fun i ->  print_string ((string_of_int i) ^ " ")) lst;
      print_newline ();
      List.iter (fun i ->  print_string ((string_of_int i) ^ " ")) lst_sorted;
      print_newline ()
    end

let _ = main ()
