# Strolch WebSocket

The `strolch-websocket` module provides a real-time observation API for Strolch elements. It allows clients to receive updates when Resources, Orders, or Activities are added, updated, or removed.

## Endpoint

The default WebSocket endpoint is:

```
ws://<host>:<port>/<context>/websocket/strolch/observer
```

The endpoint is configured using `WebSocketConfigurator` to capture the remote IP and other connection details.

## Protocol

Communication is performed using JSON messages. Every message must contain a `msgType` field.

### Authentication

Clients must authenticate immediately after connecting. Until authenticated, no other messages will be processed.

**Request:**
```json
{
  "msgType": "Authenticate",
  "username": "admin",
  "authToken": "d8e8f8..."
}
```

**Response (Success):**
```json
{
  "msgType": "Authenticate",
  "msg": "-"
}
```

If authentication fails, the server will close the connection with a protocol error.

### Observer Registration

After authentication, clients can register to observe changes for specific object types and sub-types.

**Request:**
```json
{
  "msgType": "ObserverRegister",
  "objectType": "Resource",
  "type": "Ball",
  "realm": "strolch_admin",
  "params": {
    "flat": true,
    "withLocator": true,
    "withVersion": true
  }
}
```

- `objectType`: The Strolch element type (`Resource`, `Order`, `Activity`).
- `type`: The specific sub-type, or `*` for all types of that `objectType`.
- `realm`: (Optional) The Strolch realm. Defaults to the user's realm or `default_realm`.
- `params`: (Optional) Serialization parameters:
    - `flat`: If `true`, serializes the element in a flat format.
    - `withLocator`: If `true`, includes the element's locator.
    - `withVersion`: If `true`, includes the element's version.

### Observer Unregistration

To stop receiving updates for a specific type:

**Request:**
```json
{
  "msgType": "ObserverUnregister",
  "objectType": "Resource",
  "type": "Ball",
  "realm": "strolch_admin"
}
```

### Server Updates

When an observed element changes, the server sends an update message to the client.

**Message Type:** `ObserverAdd`, `ObserverUpdate`, `ObserverRemove`

**Example:**
```json
{
  "msgType": "ObserverUpdate",
  "objectType": "Resource",
  "type": "Ball",
  "msg": "-",
  "data": [
    {
      "id": "b1",
      "type": "Ball",
      ...
    }
  ]
}
```

The `data` field contains a JSON array of the elements that were changed.

## Technical Implementation

- `WebSocketEndpoint`: The Jakarta WebSocket entry point. It creates a `WebSocketClient` for each session.
- `WebSocketClient`: Handles the message loop, authentication, and manages realm-specific observer handlers.
- `WebSocketObserverHandler`: Implements the Strolch `Observer` interface and translates Strolch events into JSON messages sent over the WebSocket.
- `WebSocketConfigurator`: Custom configurator to extract remote IP information, including support for `X-Forwarded-For` headers.
