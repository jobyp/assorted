let random_array n =
  let a = Array.of_list (Util.range n) in
    (Jsort.shuffle a; a)

let main () = 
  let n = if Array.length (Sys.argv) = 2 then int_of_string (Sys.argv.(1)) else 10 in
  let a = random_array n in
  begin
    print_string "random_array: ";
    Array.iter (fun i ->  print_string ((string_of_int i) ^ " ")) a;
    print_newline ();
    Jsort.merge_sort_i  a;
    print_string "sorted_array: ";
    Array.iter (fun i ->  print_string ((string_of_int i) ^ " ")) a;
    print_newline ()
  end

let _ = main ()
