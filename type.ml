type ('a, 'b) v = ('a * 'b)
[@@deriving to_protocol ~driver:(module Json), sexp]
(*
{pexp_desc =
  Pexp_let (Nonrecursive,
   [{pvb_pat =
      {ppat_desc =
        Ppat_constraint ({ppat_desc = Ppat_var {txt = "f"}},
         {ptyp_desc =
           Ptyp_poly ([],
            {ptyp_desc =
              Ptyp_arrow (Nolabel,
               {ptyp_desc =
                 Ptyp_arrow (Nolabel, {ptyp_desc = Ptyp_var "a"},
                  {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])})},
               {ptyp_desc =
                 Ptyp_arrow (Nolabel, {ptyp_desc = Ptyp_var "b"},
                  {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])})})})})};
     pvb_expr =
      {pexp_desc =
        Pexp_constraint
         ({pexp_desc =
            Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "a"}},
             {pexp_desc =
               Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "b"}},
                {pexp_desc = Pexp_ident {txt = Lident "a"}})})},
         {ptyp_desc =
           Ptyp_arrow (Nolabel,
            {ptyp_desc =
              Ptyp_arrow (Nolabel, {ptyp_desc = Ptyp_var "a"},
               {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])})},
            {ptyp_desc =
              Ptyp_arrow (Nolabel, {ptyp_desc = Ptyp_var "b"},
               {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])})})})}}],
   {pexp_desc = Pexp_ident {txt = Lident "f"}})}

*)
(*
let here = "==============================================================="
let here = "==============================================================="
let here = "==============================================================="
let here = "==============================================================="
type u = A | B of int | C of string * float | D of (int * int) | E of { x: int }
[@@deriving protocol ~driver:(module Json)]
*)
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
