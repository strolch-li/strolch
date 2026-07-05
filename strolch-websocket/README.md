# Strolch WebSocket

The `strolch-websocket` module provides a WebSocket API for Strolch. It allows clients to observe changes to Strolch
elements (Resources, Orders, Activities) in real-time.

For more technical details and usage instructions, see [docs/strolch-websocket.md](docs/strolch-websocket.md).

## Usage

To use the WebSocket API, you need to include the `strolch-websocket` dependency in your project and ensure that the
WebSocket endpoint is correctly registered in your web container.

### Maven Dependency

```xml
<dependency>
    <groupId>li.strolch</groupId>
    <artifactId>strolch-websocket</artifactId>
    <version>${strolch.version}</version>
</dependency>
```

### Connection

Connect to the WebSocket endpoint at `/websocket/strolch/observer` and perform authentication as described in the
[technical documentation](docs/strolch-websocket.md).
