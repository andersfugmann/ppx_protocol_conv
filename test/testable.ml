module type Driver = Protocol_conv.Runtime.Driver

module type Test = functor(Driver: Driver) -> sig
  val unittest : printer:(Driver.t -> string) -> unit Alcotest.test
end

module Make (Driver: Driver) = struct
  module type Testable = sig
    type t [@@deriving protocol ~driver:(module Driver), sexp]
    val t: t
    val name: string
  end

  let test (module T : Testable) =
    let f () =
      let serialized = T.to_driver T.t in
      let out_ch = open_out_gen [Open_append] 0o644 "unittest.output" in
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
      (* Need to create a formatter:  *)
      let fmt : T.t Fmt.t = fun formatter t ->
        Format.fprintf formatter "%s" (Base.Sexp.to_string_hum (T.sexp_of_t t))
      in
      Alcotest.(check (of_pp fmt)) T.name T.t t'
    in
    Alcotest.test_case T.name `Quick f
end
