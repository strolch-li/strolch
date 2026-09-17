# Personal Access Tokens (PATs)

Personal Access Tokens (PATs) provide a mechanism for users to authenticate and authorize actions without requiring a standard session-based login. They are intended for programmatic access, scripts, and long-lived integrations.

## Overview

A Personal Access Token is tied to a specific user but has its own identity, scope (privileges), and lifecycle. 

### Key Features
- **Restricted Scope**: PATs are created with a specific set of privileges. When authenticating with a PAT, the resulting `Certificate` only contains these privileges, regardless of the user's full set of roles.
- **Expiration**: PATs have a `validFrom` and `validTo` date.
- **Revocation**: PATs can be revoked independently of the user's password.
- **Last Used Tracking**: The system tracks when a PAT was last used for authentication.

## Data Model

The PAT implementation consists of the following components:

### PersonalAccessToken (Internal Model)
Stored in the `PersistenceHandler`.
- `tokenId`: Unique identifier (UUID).
- `username`: The user who owns the token.
- `name`: A descriptive name for the token.
- `passwordCrypt`: Securely hashed token value (PBKDF2).
- `validFrom`: Start of validity.
- `validTo`: End of validity.
- `lastUsed`: Timestamp of the last successful authentication.
- `privileges`: Map of `Privilege` objects defining the restricted scope.

### PersonalAccessTokenRep (Representation)
Used for API communication.
- Similar fields to `PersonalAccessToken`, but excludes sensitive hash data and includes a list of `Privilege` objects.

## Logic and Services

### PrivilegeHandler
The core interface for PAT management:
- `getPersonalAccessTokens(Certificate certificate)`: List tokens for the current user.
- `getPersonalAccessTokens(Certificate certificate, String username)`: List tokens for the given user.
- `createPersonalAccessToken(Certificate certificate, String name, ZonedDateTime validFrom, ZonedDateTime validTo, Set<String> roles, List<Privilege> privileges)`: Create a new token for the current user. Requires `PrivilegePersonalAccessToken`.
  - The new token's privileges are a snapshot of the user's *current* privileges at the time of creation, optionally filtered by the given roles and/or privileges. 
  - If both roles and privileges are null or empty, then all of the user's current privileges are assigned to the token.
- `removePersonalAccessToken(Certificate certificate, String tokenId)`: Revoke a token.
- `authenticatePersonalAccessToken(String token, String source)`: Authenticate using a raw token string. The expected format is `tokenId:tokenValue`.

### Authentication Flow
1. The user provides the raw token (format `tokenId:tokenValue`).
2. The `PrivilegeHandler` extracts the `tokenId` and the `tokenValue`.
3. It checks a local cache for a valid `PrivilegeContext` associated with the `tokenId`.
   - The token must still exist in the persistence layer (revocation check).
   - The token must still be within its validity period.
   - The associated user must still be enabled.
   - The provided `tokenValue` is verified against a cached fast cryptographic hash ($O(1)$ SHA-256).
4. If a valid cached context is found and the token secret matches, it is returned immediately, skipping expensive cryptographic PBKDF2 hashing.
5. If no valid cache entry exists:
   - It looks up the `PersonalAccessToken` by its `tokenId` in the `PersistenceHandler`.
   - It hashes the provided `tokenValue` using PBKDF2 and compares it with the stored hash.
   - It verifies the token's validity and the user's state.
   - A new `PrivilegeContext` is built and added to the cache along with the fast verification hash.
   - The `lastUsed` timestamp is updated in the persistence layer.
6. A `Certificate` with `Usage.API` is returned.

### Cache Pruning
To maintain performance while managing memory, the PAT cache is pruned by a background task that runs every minute. 
- A cached `PrivilegeContext` is removed from the cache if it hasn't been accessed for more than 10 minutes (idle timeout).
- Every access to a cached PAT context updates its last access time.

## REST & HTTP Authentication

PATs can be used across multiple HTTP authentication headers in Strolch REST APIs:

1. **HTTP Basic Auth**: Legacy clients and automation scripts can pass PATs in standard Basic Auth headers (`Authorization: Basic <base64>`):
   - `base64(tokenId:tokenValue)` (Token ID in username, token secret in password)
   - `base64(username:tokenId:tokenValue)` (Username in username, full PAT in password)
   - `base64(tokenId:tokenValue:)` (Full PAT in username)
2. **Bearer Auth**: Standard modern header `Authorization: Bearer <tokenId:tokenValue>`.
3. **Direct Authorization Header**: `Authorization: <tokenId:tokenValue>`.

## Integration Points

### REST API Specification

#### Base URL
`{server_url}/rest/strolch/privilege/tokens`

#### Authentication
All requests require a valid Strolch authentication token (session token) passed in the `Authorization` header.
- **Header**: `Authorization: <session_token>`

---

#### 1. List Personal Access Tokens
Retrieves all tokens belonging to the authenticated user.

- **Method**: `GET`
- **Path**: `/`
- **Produces**: `application/json`
- **Response Body**: `Array<PersonalAccessTokenRep>`

**Response Example**:
```json
[
  {
    "tokenId": "550e8400-e29b-41d4-a716-446655440000",
    "username": "jdoe",
    "name": "Integration Script",
    "validFrom": "2024-01-01T00:00:00.000Z",
    "validTo": "2024-12-31T23:59:59.000Z",
    "lastUsed": "2024-06-02T10:00:00.000Z",
    "privileges": [
      {
        "name": "li.strolch.service.api.Service",
        "policy": "DefaultPrivilege",
        "allAllowed": false,
        "allowList": ["GetOrderService"],
        "denyList": []
      }
    ]
  }
]
```

---

#### 2. Create Personal Access Token
Creates a new token with a specific name, validity period, and optional privilege subset.

- **Method**: `POST`
- **Path**: `/`
- **Consumes**: `application/json`
- **Produces**: `application/json`
- **Request Body**: `CreatePersonalAccessTokenArgument`

**Request Body Schema**:
- `name` (String, required): Descriptive name for the token.
- `validFrom` (ISO8601 String, required): Start date of token validity.
- `validTo` (ISO8601 String, required): Expiry date of token.
- `roles` (Array<String>, optional): List of role names to restrict the token to.
- `privileges` (Array<Privilege>, optional): List of specific privileges to assign.

**Request Example**:
```json
{
  "name": "My New Token",
  "validFrom": "2024-06-02T00:00:00Z",
  "validTo": "2025-06-02T00:00:00Z",
  "roles": ["AppUser"],
  "privileges": []
}
```

**Response**:
- **Status**: `200 OK`
- **Body**:
```json
{
  "token": "550e8400-e29b-41d4-a716-446655440000:aB1c..."
}
```
- **Note**: This is the **only time** the `tokenValue` is returned. It must be stored securely by the client.

---

#### 3. Revoke Personal Access Token
Immediately invalidates a token by its ID.

- **Method**: `DELETE`
- **Path**: `/{tokenId}`
- **Produces**: `application/json`
- **Response Body**: `ServiceResultResponse`

**Response Example**:
```json
{
  "state": "SUCCESS",
  "message": "Token removed successfully."
}
```

---

#### Data Models

##### Privilege Object
```json
{
  "name": "string",
  "policy": "string",
  "allAllowed": "boolean",
  "allowList": ["string"],
  "denyList": ["string"]
}
```

##### PersonalAccessTokenRep
- `tokenId` (UUID String)
- `username` (String)
- `name` (String)
- `validFrom` (ISO8601 String)
- `validTo` (ISO8601 String)
- `lastUsed` (ISO8601 String)
- `privileges` (Array<Privilege>)

### Persistence
PATs are persisted via the `PersistenceHandler`. The default `XmlPersistenceHandler` stores them in `PrivilegeTokens.xml`.

### Privilege Context
The `PrivilegeContextBuilder` has been extended to support building a context from a `PersonalAccessToken`. This ensures that even if a user has many roles, the `Certificate` generated from a PAT only contains the specific privileges assigned to that token.

## Security Considerations

### Token Entropy and Generation Security
Personal Access Tokens in Strolch are generated by `DefaultEncryptionHandler.nextToken()` and adhere to high cryptographic security baselines:
- **Entropy Size**: 32 bytes (256 bits) of cryptographically secure pseudo-random data, encoded as a 64-character lowercase hexadecimal string.
- **Search Space**: $2^{256} \approx 1.15 \times 10^{77}$ possible combinations, making brute-force enumeration attacks computationally infeasible across any modern or foreseeable classical computing capabilities.
- **CSPRNG Source**: Backed by `java.security.SecureRandom`, sourcing entropy directly from underlying OS entropy pools (`/dev/urandom` / `NativePRNG` on Unix/Linux, `CryptGenRandom` / `BCryptGenRandom` on Windows).
- **Dual-Layer Identifier Design**: Every PAT consists of a UUIDv4 `tokenId` and a 256-bit secret `tokenValue` (`<tokenId>:<tokenValue>`). This separates identity lookup from secret verification and eliminates global namespace collisions.
- **Standards Compliance**: NIST SP 800-63B and OWASP guidelines recommend a minimum of 128 bits of entropy for secret authentication tokens. Strolch provides 256 bits (matching the strength of AES-256 and SHA-256).

### Comparison with Industry Standards

| Platform | Random Entropy | Encoding / Length | Generator |
| :--- | :--- | :--- | :--- |
| **Strolch PAT** | **256 bits (32 bytes)** | **64 hex characters** | **`java.security.SecureRandom`** |
| **GitHub PAT** | ~256 bits (30–32 bytes) | Base62 (e.g., `ghp_...` 36–40 chars) | CSPRNG |
| **GitLab PAT** | 160–256 bits (20–32 bytes) | Base64/Alphanumeric (e.g., `glpat-...`) | CSPRNG |
| **AWS Secret Keys** | 320 bits (40 bytes) | Base64 (40 chars) | CSPRNG |
| **OWASP Minimum Recommendation** | $\ge$ 128 bits (16 bytes) | Standard representation | CSPRNG |

### Hashing, Salting, and At-Rest Storage
- **Salt Generation**: Every token is hashed with a distinct, cryptographically random 32-byte salt generated via `SecureRandom.nextBytes()`.
- **At-Rest PBKDF2 Hashing**: Raw tokens are never stored in plaintext. In `PrivilegeTokens.xml` (or database persistence), tokens are stored as PBKDF2 hashes using `PasswordCrypt` (`$algorithm,iterations,keyLength$salt$hash`).
- **One-Time Secret Disclosure**: The raw token is returned to the user only once upon creation.
- **Fast In-Memory Verification Caching**: To prevent CPU exhaustion and latency from PBKDF2 hashing on high-frequency API calls, cached active tokens are verified using $O(1)$ constant-time SHA-256 hash checks with the token's unique salt.
- **Authorization & Scope**: Token operations are restricted to the token owner or administrators with explicit privileges. Authenticators receive a `Certificate` containing only the restricted subset of privileges configured on the token.
