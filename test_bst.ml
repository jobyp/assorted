let random_list n =
  let a = Array.of_list (Util.range n) in
    (Jsort.shuffle a; Array.to_list a)

let main () =
  let n = if Array.length (Sys.argv) = 2 then int_of_string (Sys.argv.(1)) else 10 in
  let lst = random_list n in
  let bst = Bst.from_list lst in
  let lst_sorted = Bst.to_list bst in
    begin
      print_string "random_array: ";
      List.iter (fun i ->  print_string ((string_of_int i) ^ " ")) lst;
      print_newline ();
      print_string "sorted_array: ";
      List.iter (fun i ->  print_string ((string_of_int i) ^ " ")) lst_sorted;
      print_newline ();
      print_endline "balanced_tree";
      print_endline "-------------";
      Bst.print_tree (Bst.balance bst)
    end

let _ = main ()
