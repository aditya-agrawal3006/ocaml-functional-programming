type time = Time of int * int
[@@deriving show {with_path = false}]
type booking = Booking of string * time
[@@deriving show {with_path = false}]

val is_valid_schedule : booking list -> bool

val is_optimal : booking list -> time list -> bool