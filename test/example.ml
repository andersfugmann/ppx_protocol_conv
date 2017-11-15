open Base
open Deriving_protocol_json
open Deriving_protocol_xml
type a = string * int list [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]
type aopt = a option [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

module Jsont = struct end

type y = {
  y_a: int [@key "fugmann"];
  y_b: a;
  y_c_: aopt [@key "fugmann1"];
} [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

type yb = {
  y_a: int [@key "fugmann"];
  y_b: a;
  y_c_: aopt [@key "fugmann1"];
} [@@deriving protocol ~driver:(module Xml_light)]


type x = {
  foo: int;
  bar: string;
  baz: y;
} [@@deriving protocol ~driver:(module Json), protocol ~driver:(module Xml_light)]

let v = { foo=1; bar="one"; baz={ y_a=2; y_b=("two", [10; 20; 30]); y_c_=Some ("three", [100; 200; 300]) } }

let _test_json : unit =
  let json = x_to_json v in
  Caml.Printf.printf "%s\n" (Yojson.Safe.to_string json);
  assert (Base.Poly.equal v (x_of_json json))

let _test_xml : unit =
  let xml = x_to_xml_light v in
  Caml.Printf.printf "%s\n" (Xml.to_string_fmt (Xml.Element ("Root node", [] , xml)));
  assert (Base.Poly.equal v (x_of_xml_light xml))
