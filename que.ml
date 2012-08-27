type 'a t = {front : 'a Stk.t;
             back : 'a Stk.t}

exception Queue_empty

let create () = { front = Stk.create ();
                  back = Stk.create () }


let enque q a = Stk.push q.front a

let rec copy_elements q = 
  try
    Stk.push q.back (Stk.pop q.front);
    copy_elements q
  with Stk.Stack_empty -> ()

let deque q =
  try (Stk.pop q.back)
  with Stk.Stack_empty ->
    copy_elements q;
    try (Stk.pop q.back)
    with Stk.Stack_empty -> raise Queue_empty
      


