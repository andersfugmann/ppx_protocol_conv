open Protocol_conv_json
type 'a x = 'a
and y = A of y x
[@@deriving protocol ~driver:(module Json)]
