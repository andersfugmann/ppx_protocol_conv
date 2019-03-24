type t = {
  a: int;
  b: string;
  c: char;
}
[@@deriving protocol ~driver:(module Driver)]

(*
type t = A of { a : string; }
       | B of int
       | C of { x : int; y: int; }
[@@deriving to_protocol ~driver:(module Driver)]
*)

(* Create a poly list??? *)
(*
type a = { x: int; y: (int * int) }
  (let open Protocol_conv.Runtime in
     let of_funcs =
       ("x", (fun t  -> Driver.to_int t)) ^::
         (("y",
            (let open Protocol_conv.Runtime in
               let of_funcs =
                 ("t0", (fun t  -> Driver.to_int t)) ^::
                   (("t1", (fun t  -> Driver.to_int t)) ^:: Nil)
                  in
               let constructor x0 x1 = (x0, x1)  in
               (fun t  -> Driver.to_tuple t) of_funcs constructor))
            ^:: Nil)
        in
     let constructor x y = { x; y }  in
     (fun t  -> Driver.to_record t) of_funcs constructor) t
*)
(*
type a = A of int
       | B of string * int
       | C
[@@deriving of_protocol ~driver:(module Driver)]


let rec a_of_driver t =
  ((fun t  -> Driver.to_variant t)
     (function
      | ("A",c0::[]) -> A (((fun t  -> Driver.to_int t)) c0)
      | ("B",c0::c1::[]) ->
          B
            ((((fun t  -> Driver.to_string t)) c0),
              (((fun t  -> Driver.to_int t)) c1))
      | ("C",[]) -> C
      | (s,_) -> failwith ("Unknown variant or arity error: " ^ s))) t
*)
