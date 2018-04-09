# Rutta

Download the Logary source and build.

## Router

Example `App.config`:

    <appSettings>
      <add key="subcommand" value="router" />
      <add key="router" value="--listener udp 127.0.0.1:20001 plain --target stackdriver://google/?projectId=your-project --target console://./" />
    </appSettings>

## On Windows

You'll need `libzmq.dll` present when running Rutta, a v4.x from http://zeromq.org/distro:microsoft-windows.

### Stackdriver

You'll need Google's DLLs which are specific to the OS you run on.
