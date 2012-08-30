let swap a i j = 
  let tmp = a.(i) in
  begin
    a.(i) <- a.(j);
    a.(j) <- tmp
  end

let sel_sort a = 
  let b = (Array.length a - 1) in (* upper bound *)
  let rec sort i = 
    if i >= b 
    then ()
    else 
      let min = ref i in
      for j = i to b 
      do
        if a.(j) < a.(!min) then min := j
      done;
      swap a i !min;
      sort (i + 1) in
  sort 0

let ins_sort a = 
  let b = Array.length a - 1 in (* upper bound *)
  let rec sort j = 
    if j < 1 then ()
    else 
      if a.(j - 1) > a.(j)
      then (swap a (j - 1) j ; sort (j - 1))
      else () in
  for i = 0 to b 
  do
    sort i
  done

let shell_sort a =
  let b = (Array.length a - 1) in (* upper bound *)
  let rec max_h i =
    if i > (b + 1) / 3 then i else max_h (3 * i + 1) in
  let rec loop j h = 
    if j < h then ()
    else
      if a.(j - h) > a.(j) 
      then (swap a (j - h) j ; loop (j - h) h)
      else () in
  let h_sort h =
    for i = h to b 
    do
      loop i h
    done in
  let rec sort h =
    if h < 1 then ()
    else (h_sort h; sort (h / 3)) in
  sort (max_h 1)
      
    
let shuffle a = 
  let l = Array.length a in
  let rec loop i =
    if i >= l then ()
    else
      let j = Random.int (i + 1) in
      swap a i j;
      loop (i + 1)
  in
  loop 0

let print_array a lo hi =
  for i = lo to hi 
  do 
    print_string ((string_of_int a.(i)) ^ " ")
  done;
  print_newline ()
  

let merge a b lo mid hi =  (* a[lo..mid] ; a[mid+1..hi] *)
  let rec loop i j k = 
    if i > mid
    then Array.blit b j a k (hi + 1 - j)
    else if j > hi
    then Array.blit b i a k (mid + 1 - i)
    else
      if b.(i) <= b.(j)
      then (a.(k) <- b.(i); loop (i + 1) j (k + 1))
      else (a.(k) <- b.(j); loop i (j + 1) (k + 1)) in
  begin
    Array.blit a lo b lo (hi + 1 - lo);
    loop lo (mid + 1) lo;
  end

let merge_sort a =
  let b = Array.copy a in
  let rec sort lo hi =
    if lo >= hi then () 
    else 
      let mid = lo + (hi - lo) / 2 in
      (sort lo mid; sort (mid + 1) hi; merge a b lo mid hi) in
  sort 0 (Array.length a - 1)
