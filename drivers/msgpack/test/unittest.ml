module Unittest = Test.Unittest.Make (Protocol_conv_msgpack.Msgpack)
let () = Unittest.run ~extra:[Test_types.tests] ~name:"msgpack" ()
