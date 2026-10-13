type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf
[@@deriving show { with_path = false }]

let rec tree_fold f init tree = 
  match tree with
    | Leaf -> init
    | Node (left, value, right) -> f (tree_fold f init left)
                                      value
                                      (tree_fold f init right)

let map tree f =   tree_fold (fun left value right ->
                              Node (left, f value, right))
                              Leaf tree
                              
let in_order tree = tree_fold (fun left value right -> 
                                left @ [value] @ right)
                                [] tree

let pre_order tree = tree_fold (fun left value right ->
                                value :: (left @ right))
                                [] tree

let compose tree = tree_fold (fun left value right ->
                                fun x ->
                                  right (value (left x)))
                              (fun x -> x) tree

let depth tree = tree_fold (fun left _ right ->
                            if left > right
                            then 1+left
                            else 1 + right)
                            0 tree

(* Assume complete tree *)
let trim tree n =
    let f = tree_fold (fun left value right ->
                        fun depth -> if depth <=0 
                                      then Leaf
                                      else Node (left (depth -1),
                                                  value,
                                                  right (depth - 1)))
                      (fun _ -> Leaf) tree
    in f n

