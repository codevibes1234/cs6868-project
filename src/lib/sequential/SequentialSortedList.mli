(** Sequential sorted list interface *)

(** The type representing the state of the sorted list *)
type 'a state = 'a list

(** The type representing operations on the sorted list *)
type 'a op =
  | Insert of 'a
  | Remove of 'a
  | Contains of 'a

(** [apply state op] applies the operation [op] to the sorted list [state], returning
    the new state and the result of the operation. *)
val apply : 'a state -> 'a op -> 'a state * 'a option

(** [empty] is the empty sorted list *)
val empty : 'a state

(** [insert x state] inserts element [x] into the sorted list [state] *)
val insert : 'a -> 'a state -> 'a state

(** [remove x state] removes element [x] from the sorted list [state] *)
val remove : 'a -> 'a state -> 'a state
