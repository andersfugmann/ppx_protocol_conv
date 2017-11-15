# Ppx Deriving Protocol
Ppx deriving protocol (de)serialisers using deriving, which allows for plugable (de)serialisers.

## Usage
```ocaml
open Deriving_protocol
type a = {
  this_is_ocaml: int;
  its_not_javascript: string [@key "notJavascript"]
} [@@deriving protocol ~driver:(module Json) ~flags:(`Mangle Json.mangle)
```

will generate the functions:
```ocaml
val a_to_json: a -> Json.t
val a_of_json: Json.t -> a
```

```ocaml
a_to_json { this_is_ocaml=42; its_not_javascript:"really" }
```
Evaluates to
```ocaml
[ "thisIsOcaml", `Int 42; "notJavascript", `String "really"] (* Yojson.Safe.json *)
```

Flags can be specified using the driver argument ~flags. For the json module, the `mangle` function transforms record label names to
be lower camelcase: a_bc_de -> aBcDe and a_bc_de_ -> aBcDe. Beware that this may cause name collisions, which can only be determined at compile time.

## Attributes
The deriver allows tag `[@key <string>]` on record declarations, which allows overriding default record label names.
the key can be prefixed with the name of the driver (lowercased), e.g. `[@json.key "new key"]`

## Signatures
The protocol deriver works also handles signature, but disallows `[@key ...]` and `~flags:...` as these does not impact signatures.

## Drivers
The protocol deriver currently only implements the `Json` driver, which serialises and de-serialises to the type `Yojson.Safe.t`

### Custom drivers
A protocol driver must implement the signature

```ocaml
include Lib.Driver with type t = ... and type flags = ...
```

## Limitations
The protocol deriver currently does not support sum types or parameterised types.
