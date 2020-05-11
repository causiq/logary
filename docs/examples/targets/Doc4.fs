use rootSpan = logger.buildSpan () |> Span.setMessage (eventX "root span") |> Span.start

do! eventX "before some action: {userId}" >> setField "userId" 123 |> logger.infoWithBP

// do some action : ...

do! eventX "after some action: {orderId}" >> setField "orderId" 321 |> logger.infoWithBP

let conStr = "Host=;Database=;Username=;Password=;"

use childSpan = Span.create logger |> Span.setMessage (eventX "child span" >> tag "DB Query" >> tag "Postgresql" >> setContext "conn str" conStr) |> Span.start

let sql = "select count(*) from xxx"
do! eventX "query : {sql}" >> setField "sql" sql >> setTimestamp (Instant.FromUnixTimeSeconds 1L) |> logger.infoWithBP