# Strolch Web Rest

RESTful Web Service API for [Strolch](https://strolch.li).

## Overview

This module provides a comprehensive RESTful API for interacting with a Strolch agent. It includes endpoints for authentication, model manipulation, inspection, reporting, and more.

## Features

- **Authentication**: Login, logout, and session management.
- **Model Manipulation**: CRUD operations for Resources, Orders, and Activities.
- **Inspection**: Inspect the state of the Strolch agent, including components, realms, and versions.
- **Reporting**: Execute and manage Strolch reports.
- **Auditing**: Query audit trails.
- **Privilege Management**: Manage users, roles, and privileges.
- **Jobs**: Manage and execute Strolch jobs.
- **I18n**: Retrieve internationalization messages.

## Configuration

The `RestfulStrolchComponent` is configured in the `StrolchConfiguration.xml`. The following parameters are supported:

| Parameter | Type | Default | Description |
| --- | --- | --- | --- |
| `corsEnabled` | Boolean | `false` | Enables Cross-Origin Resource Sharing (CORS). |
| `corsOrigin` | String | | The allowed origin for CORS. |
| `restLogging` | Boolean | `false` | Enables logging of REST requests. |
| `restLoggingEntity` | Boolean | `false` | Enables logging of REST request/response entities. |
| `httpCacheMode` | String | `No-Cache` | Sets the HTTP cache mode for responses. |
| `secureCookie` | Boolean | `true` | Enables secure flag on the authorization cookie. |
| `cookieMaxAge` | Integer | `86400` (1 day) | Max age of the authorization cookie in seconds. |
| `domain` | String | | The domain for the authorization cookie. |
| `path` | String | | The path for the authorization cookie. |
| `basicAuthEnabled` | Boolean | `true` | Enables Basic Authentication. |
| `hideVersionFromUnauthorizedClients` | Boolean | `false` | Hides the version endpoint from unauthorized clients. |
| `restTracing` | String | `OFF` | Jersey tracing support (`OFF`, `ON_DEMAND`, `ALL`). |
| `restTracingThreshold` | String | `TRACE` | Jersey tracing threshold (`SUMMARY`, `TRACE`, `VERBOSE`). |

## Documentation

Detailed technical documentation can be found in the [docs/](docs/) directory:

- [Authentication](docs/authentication.md)
- [REST Resources](docs/resources.md)
