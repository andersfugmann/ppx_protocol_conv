class type restricted_point_type =
  object
    method get_x : int
    method bump : unit
  end
[@@deriving protocol ~driver:(module Driver)]
