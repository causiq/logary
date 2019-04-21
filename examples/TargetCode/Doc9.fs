type Tenant =
  { tenantId : string
    permissions : string }

let exnMsg =
  Message.event Error "Unhandled exception"
  |> Message.setSimpleName "A.B.C"
  |> Message.setFieldFromObject "tenant" { tenantId = "12345"; permissions = "RWX" }
  |> Message.setContextFromMap (Map
    [ "user", box (Map
        [ "name", box "haf"
          "id", box "deadbeef234567"
        ])
    ])
  |> withException Message.addExn