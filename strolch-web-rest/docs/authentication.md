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
Or directly provide a Personal Access Token:
```
Authorization: <tokenId:tokenValue>
```

### 3. Bearer Authentication
The client can authenticate using standard Bearer tokens, supporting Personal Access Tokens (PATs) and session tokens:
```
Authorization: Bearer <tokenId:tokenValue>
```
or
```
Authorization: Bearer <session-id>
```

### 4. Basic Authentication
If enabled in the configuration (`basicAuthEnabled`), the API supports HTTP Basic Authentication:
```
Authorization: Basic <base64-encoded-credentials>
```

#### Standard Credentials
When using standard username/password credentials (`base64(username:password)`), a new session is created for the request and invalidated afterwards (Usage: `SINGLE`). Every request executes password hashing.

#### Personal Access Tokens (PATs) via Basic Auth
To support legacy applications, ETL/EAI scripts, and integrations that only support HTTP Basic Auth without incurring password hashing overhead (especially when password hash iterations are set to high values), Strolch supports passing Personal Access Tokens (PATs) through Basic Authentication.

Once authenticated, PAT validations are cached with fast $O(1)$ cryptographic verification, bypassing slow PBKDF2 iterations.

The following PAT configuration formats are supported:

1. **Token ID as Username, Token Secret as Password**:
   - Username: `<tokenId>` (UUID)
   - Password: `<tokenValue>`
   - Header payload: `base64(tokenId:tokenValue)`

2. **Username and Full PAT in Password Field** (e.g., standard Git/Jira client style):
   - Username: `<username>` (e.g., `admin` or `token`)
   - Password: `<tokenId:tokenValue>`
   - Header payload: `base64(username:tokenId:tokenValue)`

3. **Full PAT in Username Field**:
   - Username: `<tokenId:tokenValue>`
   - Password: `""` (empty) or arbitrary dummy value
   - Header payload: `base64(tokenId:tokenValue:)` or `base64(tokenId:tokenValue:dummy)`

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
