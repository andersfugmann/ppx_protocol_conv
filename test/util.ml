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
