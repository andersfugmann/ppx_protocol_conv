module type Parameters = sig
  (** Map field names of records
      Mapping is done after applying [[@key]] attribute.

      Default is [identity]
  *)
  val field_name: string -> string

  (** Map variant and constructor names.
      Mapping is done after applying [[@name]] attribute.

      Default is [identity]
  *)
  val variant_name: string -> string

  (** Map constructors with no arguments to a string.
      If true, constructors without arguments are mapped to a string, instead of
      than a list containing only the constructor / variant name.

      Default is [true]
  *)
  val constructors_without_arguments_as_string: bool

  (** Omit default values from output.
      If true, default values
      are not serialized. Note that this uses polymorphic compare
      to determine if a field value is the same as the default value.

      Default is [true]
  *)
  val omit_default_values: bool

  (** Lazy evaluate lazy fields.
      If true, lazy fields are parsed eagerly.
      If false, lazy fields are parsed first when forced, which means they
      will hold the serialized structure until forced, and forcing
      might raise a parse error.

      Default is [true]
  *)
  val eager: bool

  (** Fail if unknown fields are encountered when deserialising records.

      Default is [false]
  *)
  val strict: bool
end

(** Set of default Parameters *)
module Default_parameters : Parameters

module type Driver = sig
  type t
  val to_string_hum: t -> string

  val to_list: t -> t list
  val of_list: t list -> t
  val is_list: t -> bool

  val to_alist: t -> (string * t) list
  val of_alist: (string * t) list -> t
  val is_alist: t -> bool

  val to_char: t -> char
  val of_char: char -> t

  val to_int: t -> int
  val of_int: int -> t

  val to_int32: t -> int32
  val of_int32: int32 -> t

  val to_int64: t -> int64
  val of_int64: int64 -> t

  val to_nativeint: t -> nativeint
  val of_nativeint: nativeint -> t

  val to_float: t -> float
  val of_float: float -> t

  val to_string: t -> string
  val of_string: string -> t
  val is_string: t -> bool

  val to_bool: t -> bool
  val of_bool: bool -> t

  val null: t
  val is_null: t -> bool
end

(** Helper function to convert snake case identifiers to
    camel case, e.g. a_bcd_ef -> aBcdEf
*)
val mangle: string -> string

module Make: functor (D : Driver)(P : Parameters) ->
  Protocol_conv.Runtime.Driver with type t = D.t
