module type Driver = sig
  include Protocol_conv.Runtime.Driver
  val of_driver_exn: t -> t
  val of_driver: t -> (t, error) Protocol_conv.Runtime.result
  val to_driver: t -> t
  val name: string
  val serialize: t -> string
  val deserialize: string -> t
  val sexp_of_t: t -> Sexplib.Sexp.t
end

module type Test = functor(Driver: Driver) -> sig
  val unittest : printer:(Driver.t -> string) -> unit Alcotest.test
end

module Make (Driver: Driver) = struct
  module type Testable = sig
    type t [@@deriving protocol ~driver:(module Driver), sexp_of]
    val t: t
    val name: string
  end

  let test (module T : Testable) =
    let f () =
      let t = T.to_driver T.t in
      let out_ch = open_out_gen [Open_append] 0o644 "unittest.output" in
      Printf.fprintf out_ch "=== %s.%s ===\n%s\n" Driver.name T.name (Driver.to_string_hum t);
      close_out out_ch;

      let t' =
        try T.to_driver T.t |> Driver.serialize |> Driver.deserialize |> T.of_driver_exn with
        | exn ->
          Printf.printf "\n%s: Failed parsing:\n>>>>>\n%s.%s\n======\n%s\n<<<<<<\n"
            Driver.name T.name
            (Driver.to_string_hum t)
            (Base.Sexp.to_string_hum (T.sexp_of_t T.t));
          raise exn
      in
      let fmt : T.t Fmt.t = fun formatter t ->
        Format.fprintf formatter "%s" (Base.Sexp.to_string_hum (T.sexp_of_t t))
      in
      Alcotest.(check (of_pp fmt)) Driver.name T.t t'
    in
    let test_name = Printf.sprintf "%s.%s" Driver.name T.name in
    Alcotest.test_case test_name `Quick (fun () -> try f () with Failure "ignore" [@warning "-52"] -> ())
end
