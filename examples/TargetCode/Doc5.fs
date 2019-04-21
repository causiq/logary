do! eventX "after some action: {orderId}" >> setField "orderId" 321 >> setSpanId rootSpan.info.spanId |> logger.infoWithBP

// use explicitly setParentSpanInfo style, since we use hopac `do! timeOutMillis 100` before (the ambientSpanId middleware will not be guaranteed)
let childSpan = Span.create logger |> Span.setMessage (eventX "child span" >> tag "DB Query" >> tag "Postgresql" >> setContext "conn str" conStr) |> Span.setParentSpanInfo rootSpan.info |> Span.start