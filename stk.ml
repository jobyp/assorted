type 'a t = {mutable c : 'a array;
             mutable sp: int}

exception Stack_empty

let create () = {c = [||]; sp = 0}

let resize_array st a =
  let new_size =
    let len = Array.length st.c in
    if len > 0 then 2 * len else 1 
  in
  let new_array = Array.make new_size a in
  Array.blit st.c 0 new_array 0 (Array.length st.c);
  st.c <- new_array
    
let push st a = 
  if Array.length st.c = st.sp then resize_array st a else ();
  st.c.(st.sp) <- a;
  st.sp <- st.sp + 1

let shrink st =
  let len = Array.length st.c in
  if len <= 1
  then st.c <- [||]
  else 
    let new_array = Array.make (len / 2) st.c.(0) in
    Array.blit st.c 0 new_array 0 st.sp;
    st.c <- new_array
  
let pop st = 
  if st.sp = 0 
  then raise Stack_empty
  else
    st.sp <- st.sp - 1;
    let e = st.c.(st.sp) in
    if Array.length st.c >= (3 * st.sp) 
    then shrink st
    else ();
    e

let is_empty st = (st.sp = 0)

let iter f st =
  for i = (st.sp - 1) downto 0
  do
    f st.c.(i)
  done
