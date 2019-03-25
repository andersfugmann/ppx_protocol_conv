# Ppx Protocol Conv
Ppx protocol conv (de)serialisers using deriving, which allows for
plugable (de)serialisers. [Api](https://andersfugmann.github.io/ppx_protocol_conv).

[![Build Status](https://travis-ci.org/andersfugmann/ppx_protocol_conv.svg?branch=master)](https://travis-ci.org/andersfugmann/ppx_protocol_conv)

## Features
The ppx supports the following features:
 * records
 * recursive and non-recursive types
 * variants
 * polymophic variants
 * All primitive types (except nativeint)

The following drivers exists
 * `Json` which serialises to `Yojson.Safe.t`
 * `Jsonm` which serialises to `Ezjsonm.value`
 * `Xml_light` which serialises to `Xml.xml list`
 * `Msgpack` which serialises to `Msgpck.t`
 * `Yaml` which serialises to `Yaml.t`

## Example Usage
```ocaml
open Protocol_conv
open Protocol_conv_json
type a = {
  x: int;
  y: string [@key "Y"]
  z: int list [@default [2;3]]
} [@@deriving protocol ~driver:(module Json)]

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
a_to_json { x=42; y:"really"; z:[6;7] }
```
Evaluates to
```ocaml
[ "x", `Int 42; "Y", `String "really"; "z", `List [ `Int 6; `Int 7 ] ] (* Yojson.Safe.json *)
```

`to_protocol` deriver will generate serilisation of the
type. `of_protocol` deriver generates de-serilisation of the type,
while `protocol` deriver will generate both serilisation and de-serilisation functions.

## Attributes
Record label names can be changed using `[@key <string>]`

Variant and polymorphic variant constructors names can also be changed using the `[@name <string>]`
attribute.

If a record field is not present in the input when deserialising, as default value can be
assigned using `[@default <expr>]`. If the value to be serialized
matches the default value, the field will be omitted. Comparrison uses
polymorphic compare, so be carefull.

## Signatures
The ppx also handles signature, but disallows
`[@key ...]`, `[@default ...]` and `[@name] ....` as these does not impact signatures.

## Drivers

### Notes on type mappings
All included driver allow for the identity mapping by using the
`<driver>.t` type, i.e.:
```ocaml
type example = {
  json: Json.t; (* This has type Yojson.Safe.t *)
}
```
#### Json
Maps to and from `Yojson.Safe.t`

##### Options
Standard options mimics that of `ppx_deriving_yojson`, except that
Constructors without arguments are serialized to a string rather than
a list.

Complete yojson compatibility with `ppx_deriving_yojson` it provides
though module Json.Yojson.

##### Types

| Ocaml type         | Generates     | Accepts       |
|--------------------|---------------|---------------|
| string, char,bytes | \`String      | \`String      |
| int, int32, int64  | \`Int         | \`Int         |
| float              | \`Float       | \`Float       |
| bool               | \`Bool        | \`Bool        |
| unit               | \`List []     | \`List []     |
| 'a list, 'a array  | \`List 'a list| \`List 'a list |
| 'a option          | \`Null or \'a | \`Null or \'a |
| 'a ref, lazy 'a    | 'a            | 'a            |
| Json.t             | Yojson.Safe.t | Yojson.Safe.t |

#####  Notes
Serialization differs from `ppx_deriving_yojson` when serializing in
that constructors without arguments are serialized to strings, rather
than a list. Constructors with arguments are serialized to lists.

This allows for deserialising a string directly into a ADT:

```ocaml
type country = Denmark | France
and t = {
  name: string;
  country: country;
}  [@@deriving protocol ~driver:(module Json)]

{ name = "Anders"; country = Denmark } |> to_json |> Yojson.Safe.to_string
```
produces: `{ "name": "Anders", "country": "Denmark" }`

#### Jsonm
Converts to and from `Ezjsonm.value`.
Types and arguments are the same
as for the Json implementation.

#### Msgpack
Msgpack driver maps to and from `Msgpck.t`.
To allow more finegrained control over generated type, the
msgpack module defines extra types. See table in #types section.

##### Options
The module Also provides means for chaning default attribute behaviour
and record field naming convensions, by using the functor `Msgpack.Make(P:Parameters)`

##### Types

| Ocaml type         | Generates     | Accepts                           |
|--------------------|---------------|-----------------------------------|
| string             | String        | String, Bytes                     |
| bytes              | Bytes         | String, Bytes                     |
| char               | String        | String, Bytes                     |
| int                | Int           | Int, Int32, Int64, Uint32, Uint64 |
| int32              | Int32         | Int32                             |
| int64              | Int64         | Int64                             |
| float              | Float64       | Float64, Float32                  |
| unit               | List []       | List []                           |
| bool               | Bool          | Bool                              |
| 'a list, 'a array  | List 'a list  | `List 'a list                     |
| 'a option          | Nil or 'a     | Nil or 'a                         |
| 'a ref, lazy 'a    | 'a            | 'a                                |
| Msgpack.uint32     | Uint32        | Uint32                            |
| Msgpack.uint64     | Uint64        | Uint64                            |
| Msgpack.bytes      | Bytes         | Bytes, String                     |
| Msgpack.float32    | Float32       | Float32                           |
| Msgpack.t          | MsgPck.t      | MsgPck.t                          |

#### Yaml
Converts to and from `Yaml.value`

##### Options
The module Also provides means for chaning default attribute behaviour
and record field naming convensions, by using the functor `Yaml.Make(P:Parameters)`

##### Types

| Ocaml type          | Generates     | Accepts   |
|---------------------|---------------|-----------|
| string, char, bytes | \`String      | \`String  |
| int,int32,int64     | \`Float       | \`Float*  |
| float               | \`Float       | \`Float   |
| bool                | \`Bool        | \`Boolt   |
| unit                | \`List []     | \`List [] |
| 'a list, 'a array   | \`List 'a list| \`List 'a list |
| 'a option           | \`Null or \'a | \`Null or \'a |
| 'a ref, lazy 'a     | 'a            | 'a            |
| Yaml.t              | Yaml.t        | Yaml.t    |

(*) Expects `abs(round(f) - f) < 0.000001`

## Custom drivers
It is easy to provide custom drivers by implementing the signature:

```ocaml
include Protocol_conv.Runtime.Driver with
  type t = ...
```
See the `drivers` directory for examples on how to implemented new drivers.
Submissions of new drivers are more than welcome.

## Not supported
* Generalised algebraic datatypes
* Extensible types
* Extensible polymorphic variants
* nativeint
