open Protocol_conv_json
open Protocol_conv_xml
open Protocol_conv_msgpack

let test_json name to_json of_json t =
  Printf.printf "json %s: %!" name;
  let json = to_json t in
  Printf.printf "%s%!" (Yojson.Safe.to_string json);
  assert (Base.Poly.equal t (of_json json));
  Printf.printf " - Ok\n"

let test_xml name to_xml of_xml t =
  Printf.printf "xml: %s: %!" name;
  let xml = to_xml t in
  Printf.printf "%s%!" (Xml.to_string (Xml.Element ("Root", [] , xml)));
  assert (Base.Poly.equal t (of_xml xml));
  Printf.printf " - Ok\n"

let test_msgpack name to_msgpack of_msgpack t =
  Printf.printf "msgpack: %s: %!" name;
  let msgpack = to_msgpack t in
  assert (Base.Poly.equal t (of_msgpack msgpack));
  Printf.printf " - Ok\n"

module type Testable = sig
  type t
  val t: t
  val name: string
  val t_of_json: Json.t -> t
  val t_to_json: t -> Json.t
  val t_of_xml_light: Xml_light.t -> t
  val t_to_xml_light: t -> Xml_light.t
  val t_of_msgpack: Msgpack.t -> t
  val t_to_msgpack: t -> Msgpack.t
end

let test (module T : Testable) =
  test_json T.name T.t_to_json T.t_of_json T.t;
  test_xml T.name T.t_to_xml_light T.t_of_xml_light T.t;
  test_msgpack T.name T.t_to_msgpack T.t_of_msgpack T.t;
