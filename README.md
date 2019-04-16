# Ppx Protocol Conv
Ppx protocol conv (de)serializers using deriving, which allows for
plugable
(de)serializers. [Api](https://andersfugmann.github.io/ppx_protocol_conv).

This page contains an simple overview of functionality provided. More
information is available in the [wiki pages](https://github.com/andersfugmann/ppx_protocol_conv/wiki)

[![Build Status](https://travis-ci.org/andersfugmann/ppx_protocol_conv.svg?branch=master)](https://travis-ci.org/andersfugmann/ppx_protocol_conv)

# Table of contents
1. [Features](#features)
1. [Examples](#examples)
1. [Drivers](#drivers)
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
 * `Json` which serializes to `Yojson.Safe.t`
 * `Jsonm` which serializes to `Ezjsonm.value`
 * `Msgpack` which serializes to `Msgpck.t`
 * `Yaml` which serializes to `Yaml.t`
 * `Xml_light` which serializes to `Xml.xml list`

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
val a_of_json_exn: Json.t -> a
val a_of_json: Json.t -> (a, exn) result

val b_to_json: a -> Json.t
val b_of_json_exn: Json.t -> a
val b_of_json: Json.t -> (b, exn) result
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

 * `Json` which serializes to `Yojson.Safe.t`
 * `Jsonm` which serializes to `Ezjsonm.value`
 * `Msgpack` which serializes to `Msgpck.t`
 * `Yaml` which serializes to `Yaml.t`
 * `Xml_light` which serializes to `Xml.xml list`

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
