open Protocol_conv_json

module Test_sig : sig
  type a = int
  [@@deriving protocol ~driver:(module Json)]
end = struct
  type a = int
  [@@deriving protocol ~driver:(module Json)]
end
