module Unittest = Test.Unittest.Make (Protocol_conv_jsonm.Jsonm)
let () = Unittest.run ~name:"jsonm" ()
