# Ppx Protocol Conv
Ppx protocol conv (de)serialisers using deriving, which allows for
plugable (de)serialisers.

## Example Usage
```ocaml
open Protocol_conv
open Protocol_conv_json
type a = {
  x: int;
  y: string [@key "Y"]
} [@@deriving protocol ~driver:(module Json) ~flags:(`Mangle Json.mangle)]

type b = A of int
       | B of int [@key "b"]
       | C
[@@deriving protocol ~driver:(module Json)]
```

will generate the functions:
```ocaml
val a_to_json: a -> Json.t
val a_of_json: Json.t -> a

val b_to_json: a -> Json.t
val b_of_json: Json.t -> a
```

```ocaml
a_to_json { x=42; y:"really" }
```
Evaluates to
```ocaml
[ "x", `Int 42; "Y", `String "really"] (* Yojson.Safe.json *)
```

`to_protocol` deriver will generate serilization of the
type. `of_protocol` deriver generates de-serilization of the type,
while `protocol` deriver will generate both serilizarion and de-serilization functions.

Flags can be specified using the driver argument ~flags. For the json
module, the `mangle` function transforms record label names to be
lower camelcase: a_bc_de -> aBcDe and a_bc_de_ -> aBcDe. Beware that
this may cause name collisions, which can only be determined at
compile time.

## Attributes
Record label names can be changed using `[@key <string>]`

Variant constructors names can also be changed using the `[@key <string>]`
attribute.

## Signatures
The ppx also handles signature, but disallows
`[@key ...]` and `~flags:...` as these does not impact signatures.

## Drivers
The protocol deriver implements:
 * `Json` which serializes to `Yojson.Safe.t`
 * `Xml_light` which serializes to `Xml.xml list`
 * `Msgpack` which serializes to `Msgpck.t`

### Custom drivers
It should be easy to provide custom drivers by implementing the signature:

```ocaml
include Lib.Driver with type t = ... and type flags = ...
```

See the drivers directory for examples on how to implemented new drivers.
Submissions of useful drivers are welcome

## Limitations
The json driver will currently serialize type `t option option` as `t
option`. This means that `Some None` and `None` is both mapped to
`Null`.
