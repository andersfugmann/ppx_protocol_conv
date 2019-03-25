open Protocol_conv_json
module Unittest = Test.Unittest.Make(Json)
let () = Unittest.run ~extra:[Test_attrib.test] ~name:"json" ()
