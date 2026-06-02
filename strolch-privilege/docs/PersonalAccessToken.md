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
- `createPersonalAccessToken(Certificate certificate, String name, ZonedDateTime validFrom, ZonedDateTime validTo, Set<String> roles, List<Privilege> privileges)`: Create a new token for the current user. Requires `PrivilegeCreatePersonalAccessToken`.
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
4. If a valid cached context is found, it is returned immediately, skipping expensive cryptographic hashing.
5. If no valid cache entry exists:
   - It looks up the `PersonalAccessToken` by its `tokenId` in the `PersistenceHandler`.
   - It hashes the provided `tokenValue` and compares it with the stored hash.
   - It verifies the token's validity and the user's state.
   - A new `PrivilegeContext` is built and added to the cache.
   - The `lastUsed` timestamp is updated in the persistence layer.
6. A `Certificate` with `Usage.API` is returned.

### Cache Pruning
To maintain performance while managing memory, the PAT cache is pruned by a background task that runs every minute. 
- A cached `PrivilegeContext` is removed from the cache if it hasn't been accessed for more than 10 minutes (idle timeout).
- Every access to a cached PAT context updates its last access time.

## Integration Points

### REST API
The following endpoints are available for managing Personal Access Tokens:

| Method | Path | Description |
| --- | --- | --- |
| `GET` | `/strolch/privilege/tokens` | List all PATs for the currently authenticated user. |
| `POST` | `/strolch/privilege/tokens` | Create a new PAT. |
| `DELETE` | `/strolch/privilege/tokens/{tokenId}` | Revoke a PAT. |

#### Create PAT Request Body
```json
{
  "name": "My API Token",
  "validFrom": "2024-01-01T00:00:00Z",
  "validTo": "2025-01-01T00:00:00Z",
  "roles": ["AppUser"],
  "privileges": []
}
```
The response is the raw token string in the format `tokenId:tokenValue`. **This is the only time the token value is shown.**

### Persistence
PATs are persisted via the `PersistenceHandler`. The default `XmlPersistenceHandler` stores them in `PrivilegeTokens.xml`.

### Privilege Context
The `PrivilegeContextBuilder` has been extended to support building a context from a `PersonalAccessToken`. This ensures that even if a user has many roles, the `Certificate` generated from a PAT only contains the specific privileges assigned to that token.

## Security Considerations
- Tokens are stored as hashes using the configured `EncryptionHandler`.
- Raw tokens are only shown to the user once upon creation.
- Authorization checks ensure users can only manage their own tokens (unless they have administrative privileges).
