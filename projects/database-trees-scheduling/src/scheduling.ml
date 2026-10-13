type time = Time of int * int  (* start, end in minutes *)
[@@deriving show {with_path = false}]
type booking = Booking of string * time  (* room name, time slot *)
[@@deriving show {with_path = false}]

(*helper check if times overlap and return bool*)
let overlap t1 t2 =
    match t1,t2 with
      | Time (s1, e1), Time (s2, e2) -> s1 < e2 && s2 < e1

(*helper Iterate through bookings list and see if any booking conflicts with room and time and return bool*)
let rec conflict room time bookings = 
    match bookings with
      | [] -> false
      | Booking (r2, t2) :: t -> if room = r2 && overlap time t2
                                 then true
                                  else conflict room time t


let rec is_valid_schedule (bookings : booking list) : bool = 
    match bookings with
      | [] -> true
      | Booking (r,t) :: tail -> if conflict r t tail
                                 then false
                                  else is_valid_schedule tail


(*helper Coomparator*)
let comp_end t1 t2 = 
    match t1,t2 with
      | Time (_, e1), Time (_,e2) -> if e1 < e2 then -1
                                      else if e1 > e2 then 1 else 0

(*helper perform greedy alg*)
let rec c_int et times = 
    match times with
      | [] -> 0
      | Time (st, e) :: t -> if st >= et
                            then 1 + c_int e t
                            else c_int et t

let is_optimal (bookings : booking list) (requested : time list) : bool =
    let sorted = List.sort comp_end requested in
        let max =
            match sorted with
              | [] -> 0
              | Time (_, et) :: t -> 1 + (c_int et t)

        in List.length bookings = max