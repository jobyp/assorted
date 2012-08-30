type 'a bst = 
  | Empty
  | Node of 'a * 'a bst * 'a bst
      
let cmp x y = 
  if x < y then -1
  else if x > y then 1
  else 0
    
let rec height bst =
  match bst with
    | Empty -> 0
    | Node (_, l, r) ->
      let h_l = height l in
      let h_r = height r in
      (max h_l h_r + 1)
          
(********************************************************************
A rotation is a simple transformation of a binary tree that looks 
like this:

                               |        |
                               Y        X
                              / \      / \
                             X   c    a   Y
                             ^            ^
                            a b          b c

In this diagram, X and Y represent nodes and a, b, and c are arbitrary
binary trees that may be empty.  A rotation that changes a binary tree
of the form shown on the left to the form shown on the right is called
a "right rotation" on Y.  Going the other way, it is a "left rotation"
on X.  
**********************************************************************)

let rotate_right bst =
  match bst with
    | Empty -> Empty
    | Node(x1, Empty, r1) -> bst
    | Node(x1, Node(x2, l2, r2), r1) ->
      Node(x2, l2, Node (x1, r2, r1)) 
        
let rotate_left bst = 
  match bst with
    | Empty -> Empty
    | Node(x1, l1, Empty) -> bst
    | Node(x1, l1, Node(x2, l2, r2)) ->
      Node(x2, Node(x1, l1, l2), r2)
          
let rec find bst x = 
  match bst with
    | Empty -> Empty
    | Node(y, l, r) -> 
      let c = cmp x y in
      if c > 0 then find r x
      else if c < 0 then find l x
      else bst

let rec insert bst x = 
  match bst with
    | Empty -> Node(x, Empty, Empty)
    | Node(y, l, r) -> 
      let c = cmp x y in
      if c > 0 then 
        let new_r = insert r x in
        if new_r == r then bst else Node(y, l, new_r)
      else if c < 0 then 
        let new_l = insert l x in
        if new_l == l then bst else Node(y, new_l, r)
      else bst


(**********************************************************************
  Insertion at root
  -----------------

   An approach that will work is to perform a conventional insertion as
a leaf node, then use a series of rotations to move the new node to the
root.  For example, the diagram below illustrates rotations to move
node 4 to the root.  A left rotation on 3 changes the first tree into
the second, a right rotation on 5 changes the second into the third,
and finally a left rotation on 1 moves 4 into the root position:

            1          1             1                 4
             `._        `-..__        `-._          _.' \
                5             5           4        1     5
               / \           / \         / \    =>  `_    \
              3   6 =>      4   6 =>    3   5         3    6
              ^            /           /     \       /
             2 4          3           2       6     2
                         /
                        2

The general rule follows the pattern above.  If we moved down to the
left from a node x during the insertion search, we rotate right at x.
If we moved down to the right, we rotate left.
  
*********************************************************************)


let rec insert_at_root bst x = 
  match bst with
    | Empty -> Node(x, Empty, Empty)
    | Node(y, l, r) -> 
      let c = cmp x y  in
      if c < 0 then 
        let bst = Node(y, insert_at_root l x, r) in
        rotate_right bst
      else if c > 0 then
        let bst = Node(y, l, insert_at_root r x) in
        rotate_left bst
      else 
        bst

let rec delete bst x =
  match bst with
    | Empty -> bst
    | Node(y, l, r) ->
      let c = cmp x y in
      if c < 0 
      then Node(y, delete l x, r)
      else if c > 0 
      then Node(y, l, delete r x)
      else
        if r = Empty 
        then l 
        else 
          match (rotate_left bst) with
            | Empty -> failwith "bst delete: internal error"
            | Node(y, l, r) -> Node(y, delete l x, r)


let rec make_least_node_the_root bst = 
  match bst with
    | Empty -> Empty
    | Node(x, Empty, r) -> bst
    | Node(x, l, r) -> make_least_node_the_root (rotate_right bst)          

let rec delete_by_merge bst x =
  match bst with
    | Empty -> bst
    | Node(y, l, r) ->
      let c = cmp x y in
      if c < 0 
      then Node(y, delete_by_merge l x, r)
      else if c > 0 
      then Node(y, l, delete_by_merge r x)
      else
        match (make_least_node_the_root r) with
          | Empty -> l
          | Node(z, Empty, r) -> Node(z, l, r)
          | Node(z, l1, r) -> failwith "bst delete merge: internal error" 
