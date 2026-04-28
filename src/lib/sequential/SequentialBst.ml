type 'a tree = 
  Empty
  | Node of {l : 'a tree ; v : 'a ; r : 'a tree}

let rec to_list root =  
  match root with
  | Empty -> []
  | Node {l; v; r} -> (
    to_list l @
    [v] @
    to_list r;
  )

let rec is_present root node = 
  match root with
  | Empty -> false
  | Node{l;v;r} -> (
      let cmp = Stdlib.compare node v in
      if cmp = 0 then true
      else if cmp < 0 then is_present l node
      else is_present r node
  )

let rec insert tree elt =
  match tree with  
  | Empty -> Node{l = Empty; v = elt; r = Empty}
  | Node {l; v; r} -> (
    let cmp = Stdlib.compare elt v in
    if cmp <= 0 then Node{l = (insert l elt); v; r}
    else Node{l; v; r = (insert r elt)}
  )

let rec find_min root = 
  match root with
  | Empty -> failwith "Empty tree"
  | Node{l;v;_} -> (
      match l with
      | Empty -> v
      | left -> find_min left 
  )

let rec remove root node = 
  match root with
    | Empty -> Empty
    | Node{l;v;r} -> (
        let cmp = Stdlib.compare node v in
        if cmp < 0 then Node{l = remove l node; v; r}
        else if cmp > 0 then Node{l; v; r = remove r node}
        else (
          match (l, r) with
          | (Empty, Empty) -> Empty
          | (node, Empty) -> node
          | (Empty, node) -> node
          | (left, right) -> (
              let min_val = find_min right in
              Node {l = left; v = min_val; r = remove right min_val}
          )
        ) 
    )

let empty = Empty

type 'a state = 'a tree

type 'a op = 
  | Insert of 'a
  | Remove of 'a
  | Contains of 'a
  
let apply state op =
  match op with
  | Insert x -> (insert state x, None)
  | Remove x -> (remove state x, None)
  | Contains x -> (state, if is_present state x then Some x else None)