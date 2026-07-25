# Strolch

[![Contributors](https://img.shields.io/github/contributors/strolch-li/strolch)](https://github.com/strolch-li/strolch/graphs/contributors)
[![License](https://img.shields.io/github/license/strolch-li/strolch)](https://github.com/strolch-li/strolch/blob/master/LICENSE)
[![Commit Activity](https://img.shields.io/github/commit-activity/y/strolch-li/strolch/develop?style=flat-square)](https://github.com/strolch-li/strolch/commits/develop)
[![Forks](https://img.shields.io/github/forks/strolch-li/strolch?style=flat-square "Forks")](https://github.com/strolch-li/strolch/network/members)
[![Stars](https://img.shields.io/github/stars/strolch-li/strolch?style=flat-square "Stars")](https://github.com/strolch-li/strolch/stargazers)
[![Issues](https://img.shields.io/github/issues/strolch-li/strolch?style=flat-square "Issues")](https://github.com/strolch-li/strolch/issues)
[![Build Status](https://ci.atexxi.ch/buildStatus/icon?job=strolch)](https://ci.atexxi.ch/job/strolch/)
[![Project Map](https://sourcespy.com/shield.svg)](https://sourcespy.com/github/strolchlistrolch/)

The main repository which contains all of Strolch.

## Getting Started

Either use a version on Maven Central: https://mvnrepository.com/artifact/li.strolch/

Or install locally first:

    git clone https://github.com/strolch-li/strolch.git
    cd strolch
    mvn clean install -DskipTests

Then you can create your own project. Please read the README files in the generated projects.

To create a Strolch project see https://strolch.li/development/

## More Information

Find more about Strolch at our website: https://strolch.li

## Features

### Strolch Model

The Strolch model defines the core data structures used in the framework, including Resources, Orders, and Activities.
It provides shared Java types, serialization contracts, and a flexible hierarchy for managing data through ParameterBags
and Parameters.

For more technical details, see [strolch-model/docs/strolch-model.md](strolch-model/docs/strolch-model.md).

[parameter-memory-footprint-baseline.md](strolch-model/docs/parameter-memory-footprint-baseline.md)

### Personal Access Tokens (PATs)

Personal Access Tokens (PATs) allow users to generate long-lived credentials to perform actions without a standard login
flow. PATs can be restricted to a specific set of privileges and have their own expiration date.

#### Using PATs

Users can manage their PATs through the `PrivilegeHandler`. A PAT can be used for authentication by providing it to the
`authenticatePersonalAccessToken` method, which returns a `Certificate` with the restricted scope defined for that
token.

For more technical details,
see [strolch-privilege/docs/PersonalAccessToken.md](strolch-privilege/docs/PersonalAccessToken.md).

### Strolch Services

The `strolch-service` module provides the Service API for Strolch. It encapsulates business logic into reusable Services
and Commands, and provides specialized frameworks for execution, reporting, and migrations.

For more technical details, see [strolch-service/README.md](strolch-service/README.md).

### Temporary File Retention

Strolch provides a mechanism to automatically clear old temporary files. This is handled by the `ClearTempPathJob`,
which runs daily by default.

#### Configuration

Retention periods can be configured in `strolch.xml` using ISO-8601 durations:

- `temp.retention.default`: Default retention (default is 3 months).
- `temp.retention.<prefix>`: Specific retention for a subdirectory in the temp path.

For more details, see [docs/TemporaryFileRetention.md](docs/TemporaryFileRetention.md).

### Runtime Configuration Management

Strolch allows managing its runtime configuration and policy mappings through a unified API and REST endpoints. This
enables dynamic updates to the system behavior without requiring a restart.

#### Features

- **Unified Access**: Manage core configuration and policy mappings through `ConfigurationPolicy`.
- **REST API**: Specialized endpoints for remote configuration management.
- **Policy Discovery**: Automatic classpath scanning for new policy implementations.
- **Persistence**: Changes to policy mappings are persisted back to `StrolchPolicies.xml`.

For more technical details, see [docs/RuntimeConfigurationManagement.md](docs/RuntimeConfigurationManagement.md).

### Strolch Utils

The `strolch-utils` module is a collection of project-independent Java utility classes and helpers designed to simplify
common programming tasks. It covers areas such as Design by Contract (DBC), object modification tracking, date/time
handling, specialized collections, and various helper utilities for common Java types.

For more technical details, see [strolch-utils/docs/strolch-utils.md](strolch-utils/docs/strolch-utils.md).

### PostgreSQL Persistence

The `strolch-persistence-postgresql` module provides a PostgreSQL-based persistence implementation. It supports storing
Strolch elements in a PostgreSQL database using either XML or JSON formats. It includes automatic schema management and
high-performance connection pooling.

For more technical details and setup instructions,
see [strolch-persistence-postgresql/README.md](strolch-persistence-postgresql/README.md).

### XML Persistence

The `strolch-persistence-xml` module provides a filesystem-based XML persistence implementation. It stores each Strolch
element in its own XML file on the disk. It is suitable for smaller applications or development environments.

For more technical details and setup instructions,
see [strolch-persistence-xml/README.md](strolch-persistence-xml/README.md).

### Privilege

The `strolch-privilege` module is the Role-Based Access Control (RBAC) engine for Strolch. It handles user
authentication, session management, and authorization using extensible policies.

For more technical details and setup instructions, see [strolch-privilege/README.md](strolch-privilege/README.md).

## REST API

The `strolch-web-rest` module provides a REST API for Strolch. It allows you to interact with Strolch elements using
HTTP requests.

For more technical details and setup instructions, see [strolch-web-rest/README.md](strolch-web-rest/README.md).

## WebSocket API

The `strolch-websocket` module provides a WebSocket API for Strolch. It allows you to receive real-time updates when
Strolch elements are changed.

For more technical details and setup instructions, see [strolch-websocket/README.md](strolch-websocket/README.md).

## Build and Configuration

### Prerequisites

- Java (JDK 25 or higher recommended)
- Maven 3.6+

### Build Instructions

To build the entire project from the root:

```bash
mvn clean install
```

To skip tests:

```bash
mvn clean install -DskipTests
```
