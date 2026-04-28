(** Sequential binary search tree interface *)

(** The type representing a node in the binary search tree *)
type 'a tree = 
  | Empty
  | Node of {l : 'a tree ; v : 'a ; r : 'a tree}

(** The type representing the state of the BST *)
type 'a state = 'a tree

(** The type representing operations on the BST *)
type 'a op =
  | Insert of 'a
  | Remove of 'a
  | Contains of 'a

(** [apply state op] applies the operation [op] to the BST [state], returning
    the new state and the result of the operation. Elements are compared using
    Stdlib.compare. *)
val apply : 'a state -> 'a op -> 'a state * 'a option

(** [empty] is the empty BST *)
val empty : 'a state

(** [insert state x] inserts element [x] into the BST [state] *)
val insert : 'a state -> 'a -> 'a state

(** [remove state x] removes element [x] from the BST [state] *)
val remove : 'a state -> 'a -> 'a state

(** [is_present state x] checks if element [x] is in the BST [state] *)
val is_present : 'a state -> 'a -> bool

(** [to_list state] converts the BST [state] to a sorted list *)
val to_list : 'a state -> 'a list
