type balance_factor = int 
    
type 'a avl = 
  | Empty
  | Node of balance_factor * int * 'a avl * 'a avl


let cmp x y = 
  if x < y then -1
  else if x > y then 1
  else 0



(* 
   Balance Factor
   --------------


   When a new node n is added to an AVL tree, the balance factor of 
   n's parent must change. 

   In general, if the height of p's left subtree increase, p's
   balance factor decreases. On the other hand, if the right 
   subtree's height increases, the balance factor increases.

   Case 1. p has balance factor 0.
   
           Balance factor changes to + or -.
           Also change propogates.

   Case 2. p's shorter subtree has increased in height.
      
           If the node was added to p's shorter subtree, the tree
           became more balanced and its balance factor becomes 0.
    
   Case 3. p's taller subtree has increased in height
   
           If node was added to p's taller subtree, the balance factor
           becomes +2 or -2. 
   
   
   
*)



(* let rotate_right (avl, t) = *)
(*   match t with *)
(*     | False -> avl *)
(*     | True -> *)
(* 	match avl with *)
(* 	  | Empty -> avl *)
(* 	  | Node(b, x, l, r) ->  *)
      
(* let balance avl r =  *)
(*   match avl with *)
(*     | Empty -> avl *)
(*     | Node(b, x, l, r) -> *)
(* 	match  *)

(* let rec insert avl e = *)
(*   match avl with *)
(*     | Empty -> Node(0, e, Empty, Empty) *)
(*     | Node(-1, x, l, r) -> *)
(* 	let c = cmp e x in *)
(* 	  if c = 0  *)
(* 	  then avl *)
(* 	  else if c < 0 (\* right rotate and insert into left subtree *\) *)
(* 	  then insert (right_rotate avl) e  *)
(* 	  else (\* inserting on the right *\) *)
	      
	    
	    
