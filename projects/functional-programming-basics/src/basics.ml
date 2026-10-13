open Funs 

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let abs x = 
    if x < 0 then -x else x

let rev_tup tup = 
    let (a,b,c) = tup in (c,b,a)

let is_even x = 
    if x mod 2 = 0 then true else false

let area point1 point2 =
    let (x1,y1) = point1 in
        let (x2,y2) = point2 in
            abs(x2 - x1) * abs(y2 - y1)

let apply_tup tup f =
    let (a,b,c) = tup in
        (f a, f b, f c)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    if n = 0 then 0
    else if n = 1 then 1
    else fibonacci (n - 1) + fibonacci(n - 2)

let rec pow x p = 
    if p = 0 then 1
    else x * pow x (p - 1)

let rec log x y = 
    if y < x then 0
    else 1 + log x (y / x)

let rec gcf x y =
    if y = 0 then x
    else gcf y (x mod y)

(*****************)
(* Part 3: Lists *)
(*****************)

let reverse lst = 
    let rec helper acc lst =
    match lst with
        [] -> acc
        | (x::xs) -> helper (x::acc) xs
    in
    helper [] lst

let rec zip lst1 lst2 = 
    match lst1, lst2 with
        | (a,b) :: t1, (c,d) :: t2 -> (a,b,c,d) :: (zip t1 t2)
        | [] , _ -> []
        | _ , [] -> []    

let rec merge lst1 lst2 = 
    match lst1, lst2 with
        | [], lst2 -> lst2
        | lst1, [] -> lst1
        | h1 :: t1, h2 :: t2 -> if h1 >= h2 then h2 :: merge lst1 t2 else h1 :: merge t1 lst2


let rec is_present lst v = 
    match lst with
        [] -> false
        | h::t -> if h = v then true else is_present t v

let every_nth n lst =
    let rec helper count lst = 
        match lst with
            [] -> []
            | h :: t -> if count mod n = 0 then h :: (helper (count + 1) t) else helper (count + 1) t
    in helper 1 lst


let student_summary lst = 
    match lst with
        [] -> raise (Invalid_argument "failed student_summary") 
        | (a,b) :: t -> let rec helper name grade total count lst =
                            match lst with
                                [] -> (name, grade, total / count)
                                | (x, y) :: t1 -> if y > grade 
                                                then helper x y (total + y) (count + 1) t1
                                                else helper name grade (total + y) (count + 1) t1
                        in helper a b b 1 t


    

let rec max_func_chain init funcs =
    match funcs with
        [] -> init
        | h :: t -> let pass = max_func_chain init t in
                    let go = max_func_chain (h init) t in
                        if go > pass then go else pass

(****************)
(* Part 4: HOFS *)
(****************)

let hof_reverse lst = fold (fun acc x -> x :: acc) [] lst

let is_there lst x = fold (fun acc h -> if h = x then true else acc) false lst

let count_occ lst target = fold (fun count h -> if h = target then count + 1 else count) 0 lst

let uniq lst = fold (fun acc h -> if is_there acc h then acc else h :: acc) [] lst

let every_xth x lst = 
    let result = fold (fun (count, ans) h -> if count mod x = 0
                                                then (count + 1, h :: ans)
                                                else (count + 1, ans)) (1, []) lst
    in let (_, ans) = result in hof_reverse ans

let hof_merge lst1 lst2 = 
    let insert x lst = 
        let left = fold_right (fun h ans -> if h <= x then h :: ans else ans) lst []
        in 
        let right = fold_right (fun h ans -> if h > x then h :: ans else ans) lst []
        in
        left @ (x :: right)
    in fold (fun ans h -> insert h ans) lst1 lst2
