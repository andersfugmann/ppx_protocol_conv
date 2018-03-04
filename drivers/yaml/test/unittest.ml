module Unittest = Test.Unittest.Make (Protocol_conv_yaml.Yaml)
let () = Unittest.run ~printer:(Yaml.to_string_exn)
