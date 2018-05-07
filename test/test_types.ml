open OUnit2

module Make(Driver: Testable.Driver) = struct
  module M = Testable.Make(Driver)

    module S3 : M.Testable = struct
    let name = "S3"
    type storage_class = Standard [@key "STANDARD"]
                       | Standard_ia [@key "STANDARD_IA"]
                       | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
                       | Glacier [@key "GLACIER"]


    and content = {
      storage_class: storage_class [@key "StorageClass"];
      etag: string [@key "ETag"];
    }
    and result = {
      prefix: string option [@key "Prefix"];
      contents: content list [@key "Contents"];
    }
    and t = result
    [@@deriving protocol ~driver:(module Driver)]

    let t = { prefix = Some "prefix";
              contents = [ { storage_class = Standard; etag = "Etag" } ]
            }
  end


  module T : M.Testable = struct
    let name = "Types"

    type a = string * int list
    and aopt = a option
    and  v = Variant_one of int [@key "Variant_two1"]
           | Variant_two of string
    and y = {
      y_a: int [@key "y_a"];
      y_b: a;
      y_c_: aopt [@key "y_yc"];
      y_d_: v [@key "y_yd"];
    }
    and t = {
      foo: int;
      bar: string;
      baz: y;
      unit: unit;
      unit_option: unit option;
    }
    [@@deriving protocol ~driver:(module Driver)]

    let t = { foo=1;
              bar="one";
              baz={ y_a=2;
                    y_b=("two", [10; 20; 30]);
                  y_c_=Some ("three", [100; 200; 300]);
                  y_d_=Variant_one 1
                  };
              unit = ();
              unit_option = None;
            }
  end
  let unittest ~printer = __MODULE__ >: test_list [
      M.test (module S3) ~printer;
      M.test (module T) ~printer
    ]
end
