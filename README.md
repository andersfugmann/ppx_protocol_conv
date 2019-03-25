# Ppx Protocol Conv
Ppx protocol conv (de)serialisers using deriving, which allows for
plugable (de)serialisers. [Api](https://andersfugmann.github.io/ppx_protocol_conv).

[![Build Status](https://travis-ci.org/andersfugmann/ppx_protocol_conv.svg?branch=master)](https://travis-ci.org/andersfugmann/ppx_protocol_conv)

# Table of contents
1. [Features](#features)
1. [Examples](#examples)
1. [Drivers](#drivers)
    1. [Json](#json)
    1. [Jsonm](#jsonm)
    1. [Msgpack](#msgpack)
    1. [Yaml](#yaml)
    1. [Xml_light](#xml_light)
1. [Custom drivers](#custom-drivers)
1. [Not supported](#not-supported)

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
 * `Msgpack` which serialises to `Msgpck.t`
 * `Yaml` which serialises to `Yaml.t`
 * `Xml_light` which serialises to `Xml.xml list`

## Examples
```ocaml
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

Variant and polymorphic variant constructors names can be changed using the `[@name <string>]`
attribute.

If a record field is not present in the input when deserialising, as default value can be
assigned using `[@default <expr>]`. If the value to be serialized
matches the default value, the field will be omitted (Some drivers
allow disabling this functonality. Comparrison uses polymorphic compare, so be careful.

## Signatures
The ppx also handles signature, but disallows
`[@key ...]`, `[@default ...]` and `[@name] ....` as these does not impact signatures.

## Drivers

Drivers specify concrete serialization and deserialization.
Users of the library can elect to implement their own driver see
[custom drivers](#custom-drivers), or use predefined drivers:

 * `Json` which serialises to `Yojson.Safe.t`
 * `Jsonm` which serialises to `Ezjsonm.value`
 * `Msgpack` which serialises to `Msgpck.t`
 * `Yaml` which serialises to `Yaml.t`
 * `Xml_light` which serialises to `Xml.xml list`

### Notes on type mappings
All included driver allow for the identity mapping by using the
`<driver>.t` type, i.e.:
```ocaml
type example = {
  json: Json.t; (* This has type Yojson.Safe.t *)
}
```
#### Json
Maps to and from `Yojson.Safe.t`.
Package `ppx_protocol_conv_json`

##### Options
Standard options mimics that of `ppx_deriving_yojson`, except that
Constructors without arguments are serialized to a string rather than
a list.

To set options to create output compatible with `ppx_deriving_yojson` use
```ocaml
module Json = Json.Make(
  struct
    let field_name str = str
    let singleton_constr_as_string = false
    let omit_default_values: true
  end)
```
See [Parameters](https://andersfugmann.github.io/ppx_protocol_conv/ppx_protocol_conv/Ppx_protocol_driver/module-type-Parameters/index.html)
for a description of possible options.


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
Package `ppx_protocol_conv_jsonm`

#### Msgpack
Msgpack driver maps to and from `Msgpck.t`.
To allow more finegrained control over generated type, the
msgpack module defines extra types. See table in #types section.
Package `ppx_protocol_conv_msgpack`

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
Package `ppx_protocol_conv_yaml`
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

#### Xml_light
Converts to and from `Xml_light.xml`.
The serialization is implemented to be compatible with the xml
returned from Amazon S3 api, and the implementation is feature
complete, and the implementation tries to produce as slim xml as
possible.

Package `ppx_protocol_conv_xml_light`

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
