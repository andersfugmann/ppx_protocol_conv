(rule
 (targets unittest.output)
 (deps    unittest.exe)
 (action  (run %{deps}))
)

(alias
 (name runtest)
 (package ppx_protocol_conv_json)
 (action (diff unittest.expected unittest.output))
)
