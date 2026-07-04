
# li.strolch.persistence.postgresql

[![Build Status](http://jenkins.eitchnet.ch/buildStatus/icon?job=li.strolch.persistence.postgresql)](http://jenkins.eitchnet.ch/view/strolch/job/li.strolch.persistence.postgresql/)

This module provides the PostgreSQL persistence implementation for the Strolch framework.

## Overview
The PostgreSQL persistence handler allows Strolch to store its data model (Resources, Orders, Activities, Audits, and LogMessages) in a PostgreSQL database. It supports both XML and JSON data types for element storage and includes comprehensive schema management.

## Features
- Full support for core Strolch element types.
- Multiple realm support with individual database configurations.
- Automatic schema creation and migration.
- High-performance connection pooling via HikariCP.
- Support for XML and JSON (jsonb) storage.
- Separate archive database support.

## Documentation
- [Technical Specification](docs/technical-spec.md) - Detailed architecture and configuration guide.

## Setup

### 1. PostgreSQL Installation
Ensure you have PostgreSQL installed (version 9.4 or higher recommended for JSONB support).

### 2. Database and User Creation
Create a new database and a user with the necessary privileges:

```sql
CREATE USER strolch_user WITH PASSWORD 'strolch_pass';
CREATE DATABASE strolch_db OWNER strolch_user;
GRANT ALL PRIVILEGES ON DATABASE strolch_db TO strolch_user;
```

### 3. Strolch Configuration
Configure the `PersistenceHandler` in your `StrolchConfiguration.xml`:

```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.persistence.api.StrolchPersistenceHandler</api>
    <impl>li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler</impl>
    <Properties>
        <dataType>json</dataType>
        <allowSchemaCreation>true</allowSchemaCreation>
        <db.url>jdbc:postgresql://localhost/strolch_db</db.url>
        <db.username>strolch_user</db.username>
        <db.password>strolch_pass</db.password>
    </Properties>
</Component>
```

For detailed configuration options, see the [Technical Specification](docs/technical-spec.md).

## Development and Testing

### Running Tests
To run the tests for this module, you need a running PostgreSQL instance with three test databases: `testdb`, `testdb1`, and `testdb2`.

```sql
-- testdb
CREATE USER testuser WITH PASSWORD 'test';
CREATE DATABASE testdb OWNER testuser;
GRANT ALL PRIVILEGES ON DATABASE testdb TO testuser;

-- testdb1
CREATE USER testuser1 WITH PASSWORD 'test';
CREATE DATABASE testdb1 OWNER testuser1;
GRANT ALL PRIVILEGES ON DATABASE testdb1 TO testuser1;

-- testdb2
CREATE USER testuser2 WITH PASSWORD 'test';
CREATE DATABASE testdb2 OWNER testuser2;
GRANT ALL PRIVILEGES ON DATABASE testdb2 TO testuser2;
```

Then run the tests:
```bash
mvn test
```

## References
- [Strolch Framework](https://strolch.li)
- [PostgreSQL](https://www.postgresql.org/)
- [HikariCP](https://github.com/brettwooldridge/HikariCP)
