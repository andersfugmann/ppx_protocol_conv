(lang dune 3.18)
(name ppx_protocol_conv)
(generate_opam_files true)

(maintainers "Anders Fugmann <anders@fugmann.net>")
(authors "Anders Fugmann")
(license "BSD-3-Clause")
(source (github andersfugmann/ppx_protocol_conv))
(bug_reports "https://github.com/andersfugmann/ppx_protocol_conv/issues")
(maintenance_intent "(latest)")

(package
 (name ppx_protocol_conv)
 (synopsis "Ppx for generating serialisation and de-serialisation functions of ocaml types")
 (description
  "Ppx_protocol_conv generates code to serialize and de-serialize types. The ppx itself does not contain any protocol specific code, but relies on drivers that define serialisation and de-serialisation of basic types and structures.\n\nPre-defined drivers are available in separate packages:\nppx_protocol_conv_json (Yojson.Safe.json)\nppx_protocol_conv_jsonm (Ezjson.value)\nppx_protocol_conv_msgpack (Msgpck.t)\nppx_protocol_conv_xml-light (Xml.xml)\nppx_protocol_conv_xmlm (Xmlm.node)\nppx_protocol_conv_yaml (Yaml.value)")
 (depends
  (ocaml (>= 4.08))
  (base (>= v0.14.0))
  (ppxlib (>= 0.36.0))
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_json)
 (synopsis "Json driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for json (Yojson.Safe.json) serialization and de-serialization using the yojson library")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  (yojson (>= 1.6.0))
  ppx_expect
  ppx_inline_test
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_jsonm)
 (synopsis "Jsonm driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for json (Ezjson.value) serialization and de-serialization using the Ezjson library")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  ezjsonm
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_msgpack)
 (synopsis "MessagePack driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for message pack (Msgpck.t) serialization and deserialization using the msgpck library")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  (msgpck (>= 1.3))
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_xml_light)
 (synopsis "Xml driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for xml (Xml.t) serialization and de-serialization using the xml-light library")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  xml-light
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_xmlm)
 (synopsis "Xmlm driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for xmlm (Ezxmlm.node) serialization and de-serialization using the Ezxmlm library")
 (authors
  "Anders Fugmann <anders@fugmann.net>"
  "Nick Betteridge <lists.nick.betteridge@gmail.com>")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  ezxmlm
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))

(package
 (name ppx_protocol_conv_yaml)
 (synopsis "Yaml driver for Ppx_protocol_conv")
 (description
  "This package provides a driver for yaml (Yaml.value) serialization and de-serialization using the Yaml")
 (depends
  (ocaml (>= 4.08))
  (ppx_protocol_conv (= :version))
  (yaml (>= 2.0.0))
  (ppx_sexp_conv :with-test)
  (sexplib :with-test)
  (alcotest :with-test)))
