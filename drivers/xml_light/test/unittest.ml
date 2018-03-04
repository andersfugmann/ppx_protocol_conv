module Unittest = Test.Unittest.Make (Protocol_conv_xml.Xml_light)
let () = Unittest.run ~printer:(Xml.to_string)
