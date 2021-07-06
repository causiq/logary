namespace Logary.Services.Rutta

module Proxy =
  open System
  open fszmq
  open fszmq.Socket

  let proxy xsubBind xpubBind =
    printfn $"""Proxy usage: takes the XSUB read-socket (that PUB sockets CONNECT to) \
                and then the XPUB write-socket (that SUB sockets CONNECT to). \
                --proxy %s{xsubBind} %s{xpubBind}"""

    use context = new Context()
    use reader = Context.xsub context
    bind reader xsubBind

    use writer = Context.xpub context
    bind writer xpubBind

    printfn "%s" "Spawning proxy"
    Proxying.proxy reader writer None
    { new IDisposable with member x.Dispose () = () }