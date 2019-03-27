type v = A | B of int | C of int * int | D of (int * int)
and t = v list
[@@deriving protocol ~driver:(module Json)]
(*

type v = A of { a: int; b: string; c: char; }
       | B of { a: int; b: string; c: char; }
       | C of { a: int; b: string; c: char; }
       | D of { a: int; b: string; c: char; }
       | E of { a: int; b: string; c: char; }
[@@deriving protocol ~driver:(module Driver)]


type u = A | B of int
[@@deriving protocol ~driver:(module Driver)]
*)
(* We need to name the record in order to call it. *)
(* we can use the name of the constr *)
(* So we need the name of the to_record function *)
(* And we can apply it our-selves??? *)

(* A function should return:
   The name of the function.
   The of_record function
   The pattern??? (* The pattern should be trivial *)

   No its good!
*)
