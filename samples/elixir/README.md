# Tsung + Phoenix WebSocket Testing

This directory contains a sample Elixir Phoenix application and tsung configuration
demonstrating how to load-test Phoenix Channel WebSockets with tsung.

## The Problem

Phoenix uses a specific protocol on top of standard WebSockets (RFC 6455). Tsung's
built-in websocket support handles the raw WebSocket framing correctly, but you must
craft Phoenix Channel messages in the right format:

- **Path**: `/socket/websocket?vsn=2.0.0` (not just `/socket`)
- **Frame type**: `text` (not binary — Phoenix Channel messages are JSON)
- **Message format** (vsn 2.0.0): JSON array `[join_ref, ref, topic, event, payload]`
- **Must join a topic** before sending messages: `["1","1","topic","phx_join",{}]`
- **Heartbeat**: `[null,"ref","phoenix","heartbeat",{}]`

## Quick Start

### 1. Start the Phoenix echo server

```bash
cd samples/elixir/tsung_echo
mix deps.get
mix phx.server
```

The server runs on `http://127.0.0.1:4000` with a WebSocket endpoint at
`/socket/websocket`.

### 2. Run the tsung load test

```bash
tsung -f samples/elixir/tsung_phoenix_websocket.xml start
```

Or for development (from repo root, after `./configure && make`):

```bash
./tsung.sh -f $(pwd)/samples/elixir/tsung_phoenix_websocket.xml start
```

## How It Works

### Phoenix Echo App (`tsung_echo/`)

A minimal Phoenix app with:
- `EchoChannel` on topic `echo:*` — supports `ping`, `echo`, and `broadcast` events
- `UserSocket` at `/socket` — accepts all connections (no auth)
- Health check at `GET /api/health`

### Tsung Config (`tsung_phoenix_websocket.xml`)

The session flow:
1. **WebSocket connect** — handshake to `/socket/websocket?vsn=2.0.0`
2. **Join channel** — `phx_join` the `echo:lobby` topic
3. **Send ping** — expects `pong` in response
4. **Send echo** — expects message echoed back
5. **Heartbeat** — Phoenix keepalive
6. **Another echo** — verify sustained operation
7. **Close** — clean WebSocket close

Key config details:
- Server type is `tcp` (tsung's websocket plugin handles the upgrade)
- Session type is `ts_websocket`
- All messages use `frame="text"` (critical for Phoenix)
- `<match>` elements validate Phoenix responses

### Phoenix Channel Message Format (vsn 2.0.0)

```
[join_ref, ref, topic, event, payload]
```

| Field      | Description                                    |
|------------|------------------------------------------------|
| `join_ref` | Reference from the join message (or `null`)    |
| `ref`      | Unique message reference (string integer)      |
| `topic`    | Channel topic, e.g., `"echo:lobby"`            |
| `event`    | Event name: `"phx_join"`, `"ping"`, `"echo"`   |
| `payload`  | JSON object with event data                    |

Reply format from server:
```json
["1","2","echo:lobby","phx_reply",{"status":"ok","response":{...}}]
```

## Troubleshooting

**Connection refused**: Make sure the Phoenix app is running on port 4000.

**Handshake fails**: Check that the path includes `?vsn=2.0.0`. Without the version
parameter, Phoenix may reject the connection.

**No responses / match failures**: Ensure `frame="text"` is set on all `<websocket
type="message">` elements. Phoenix expects text frames, not binary.

**Channel join errors**: The topic must match a channel route in `UserSocket`.
The sample app routes `echo:*` topics.

## Bug Fix: rand:seed API

PR #414 replaced deprecated `random` module calls with `rand`, but used the wrong
API (`rand:seed(A,B,C)` doesn't exist). The correct API is
`rand:seed(Algorithm, {A,B,C})`. This fix is included in this branch.
