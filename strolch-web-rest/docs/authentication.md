# Authentication

Strolch Web Rest uses a session-based authentication mechanism, supported by the `AuthenticationRequestFilter`.

## Authentication Methods

### 1. Cookie-based Authentication
The client can send the authorization token in a cookie named `strolch.authorization`.

### 2. Header-based Authentication
The client can send the authorization token in the `Authorization` header.
```
Authorization: <session-id>
```

### 3. Basic Authentication
If enabled in the configuration (`basicAuthEnabled`), the API supports Basic Authentication.
```
Authorization: Basic <base64-encoded-credentials>
```
When using Basic Authentication, a new session is created for the request and invalidated afterwards (Usage: `SINGLE`).

## Unsecured Paths

Certain paths are accessible without authentication:
- `strolch/authentication`: Endpoint for logging in and out.
- `strolch/authentication/sso`: Single Sign-On endpoint.
- `strolch/version`: Returns the version of the Strolch agent.
- `strolch/languages`: Returns the supported languages.

## Session Management

Authentication is performed via the `AuthenticationResource`.

### Login
- **Endpoint**: `POST strolch/authentication`
- **Payload**: JSON with `username` and `password`.
- **Response**: A `Certificate` object containing the session ID and user information. The session ID is also set as a cookie.

### Logout
- **Endpoint**: `DELETE strolch/authentication/<session-id>`
- **Response**: 204 No Content.

## Filters

The following filters are involved in the authentication process:
- `AuthenticationRequestFilter`: Validates the session or credentials on incoming requests.
- `AuthenticationResponseFilter`: Sets the authorization cookie on successful authentication responses.
- `AccessControlResponseFilter`: Handles CORS headers.
