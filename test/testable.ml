open OUnit2
module type Driver = Protocol_conv.Runtime.Driver

module type Test = functor(Driver: Driver) -> sig
  val unittest : printer:(Driver.t -> string) -> test
end

module Make (Driver: Driver) = struct
  module type Testable = sig
    type t [@@deriving protocol ~driver:(module Driver)]
    val t: t
    val name: string
    val sexp_of_t: t -> Base.Sexp.t
  end

  let test (module T : Testable) =
    let f _ =
      let serialized = T.to_driver T.t in
      let out_ch = open_out_gen [Open_append; Open_creat] 0o644 "test.out" in
      Printf.fprintf out_ch "=== %s ===\n%s\n" T.name (Driver.to_string_hum serialized);
      close_out out_ch;

      let t' =
        try T.to_driver T.t |> T.of_driver_exn with
        | exn -> Printf.printf "\n%s: Failed parsing:\n>>>>>\n%s\n======\n%s\n<<<<<<\n"
                   T.name
                   (Driver.to_string_hum serialized)
                   (Base.Sexp.to_string_hum (T.sexp_of_t T.t));
          raise exn
      in
      let printer t = Base.Sexp.to_string_hum (T.sexp_of_t t) in
      assert_equal ~printer ~msg:T.name T.t t';
      ()
    in
    T.name >:: f
end
