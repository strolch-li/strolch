# PostgreSQL Persistence Technical Specification

## Overview
The `strolch-persistence-postgresql` module provides a PostgreSQL-based persistence implementation for the Strolch framework. It supports storing Strolch elements (Resources, Orders, Activities, Audits, and LogMessages) in a PostgreSQL database.

## Architecture

### Persistence Handler
The central component is `PostgreSqlPersistenceHandler`, which implements the `PersistenceHandler` interface. It is responsible for:
- Initializing the database connections (DataSources) for each realm.
- Managing schema versioning and migrations.
- Opening transactions.
- Providing DAOs for Strolch elements.

### Transactions
`PostgreSqlStrolchTransaction` wraps a JDBC `Connection` and manages the lifecycle of the transaction. It ensures that all operations within the transaction are performed using the same connection and that the transaction is committed or rolled back appropriately.

### Data Access Objects (DAOs)
Each Strolch element type has a corresponding DAO implementation:
- `PostgreSqlResourceDao`
- `PostgreSqlOrderDao`
- `PostgreSqlActivityDao`
- `PostgreSqlAuditDao`
- `PostgreSqlLogMessageDao`

These DAOs handle the mapping between Strolch objects and the database schema.

### Data Types
Strolch elements can be stored in the database as either XML or JSON. This is controlled by the `dataType` configuration property.
- **XML**: Elements are stored in an `xml` column.
- **JSON**: Elements are stored in a `json` column.

## Schema Management
Strolch uses a custom schema management system implemented in `DbSchemaVersionCheck`. It supports:
- **Schema Creation**: Automatically creating the database schema if it doesn't exist.
- **Schema Migration**: Automatically applying migration scripts to update the schema to the latest version.
- **Schema Drop**: Dropping the schema (useful for testing).

Schema scripts are located in `src/main/resources` and follow a naming convention:
- `strolch_db_schema_<version>_initial.sql`
- `strolch_db_schema_<version>_migration.sql`
- `strolch_db_schema_<version>_drop.sql`

Similarly for the archive schema:
- `archive_db_schema_<version>_initial.sql`
- ...

## Database Schema

The database schema consists of tables for Strolch elements, audits, and logging.

### Custom Types (Enums)
- `order_state`: `CREATED`, `PLANNING`, `PLANNED`, `EXECUTION`, `STOPPED`, `WARNING`, `ERROR`, `EXECUTED`, `CLOSED`
- `access_type`: `READ`, `CREATE`, `UPDATE`, `DELETE`, `EXECUTE`
- `log_severity_type`: `Info`, `Notification`, `Warning`, `Error`, `Exception`, `System`
- `log_state_type`: `Active`, `Inactive`, `Information`

### Core Element Tables
Resources, Orders, and Activities share a similar structure. They are versioned, and the primary key is a combination of `type`, `id`, and `version`.

#### `resources`
Stores Strolch Resource elements.
- `id`, `type`, `version`: Primary Key.
- `created_by`, `created_at`, `updated_at`: Audit columns.
- `deleted`, `latest`: Versioning flags.
- `name`: Element name.
- `asxml`: XML representation (if `dataType=xml`).
- `asjson`: JSON representation (if `dataType=json`).

#### `orders`
Stores Strolch Order elements.
- Inherits columns from `resources`.
- `state`: The `order_state` enum.
- `date`: The order's date.

#### `activities`
Stores Strolch Activity elements.
- Inherits columns from `resources`.
- `state`: The `order_state` enum.

### Audit and Log Tables

#### `audits`
Stores audit trails of element access.
- `id`: Primary Key (bigint).
- `username`, `date`: Who and when.
- `element_type`, `element_sub_type`, `element_accessed`: What was accessed.
- `action`, `access_type`: How it was accessed.
- `new_version`: The new version timestamp (if applicable).
- `additional_data`: JSON field for extra information.

#### `operations_log`
Stores log messages for operations.
- `id`: Primary Key.
- `realm`: Strolch realm.
- `dateTime`, `username`: Timestamp and user.
- `severity`, `state`: Log severity and state enums.
- `locator`, `bundle`, `key`: Strolch locator and localization details.
- `message`, `stacktrace`: Log content.

#### `operations_log_values`
Stores additional key-value pairs for log messages.
- `id`: Reference to `operations_log.id`.
- `key`, `value`: The data pair.

### Archive Tables
Archived elements are stored in tables with the `archive_` prefix, typically following the same structure as the main tables but without the audit-only tables.
- `archive_resources`
- `archive_orders`
- `archive_activities`

### Versioning
The `db_version` table is used by Strolch to track the current schema version and applied migrations for both the main and archive schemas.
- `id`: Primary Key (serial).
- `app`: The application name (e.g., 'strolch' or 'archive').
- `version`: The schema version string.
- `description`: Description of the version or migration.
- `created`: Timestamp when the version was recorded.

## Configuration

The `PostgreSqlPersistenceHandler` is configured in the `StrolchConfiguration.xml` file.

### Component Configuration
```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.persistence.api.StrolchPersistenceHandler</api>
    <impl>li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler</impl>
    <Properties>
        <dataType>xml</dataType> <!-- or json -->
        <allowSchemaCreation>true</allowSchemaCreation>
        <allowSchemaMigration>true</allowSchemaMigration>
        <allowSchemaDrop>false</allowSchemaDrop>
        <allowDataInitOnSchemaCreate>true</allowDataInitOnSchemaCreate>
        
        <!-- Database Connection Properties (Default Realm) -->
        <db.url>jdbc:postgresql://localhost/testdb</db.url>
        <db.username>testuser</db.username>
        <db.password>test</db.password>
        
        <!-- HikariCP Pool Configuration -->
        <db.pool.maximumPoolSize>10</db.pool.maximumPoolSize>
        <db.pool.connectionTimeout>30000</db.pool.connectionTimeout>
    </Properties>
</Component>
```

### Multi-Realm Configuration
Realms can have their own database configuration by appending the realm name to the property key:
```xml
<db.url.myrealm>jdbc:postgresql://localhost/myrealmdb</db.url.myrealm>
<db.username.myrealm>myrealmuser</db.username.myrealm>
<db.password.myrealm>myrealmpass</db.password.myrealm>
```

### Environment Variables
If `db.useEnv` is set to `true`, the component will look for environment variables instead of properties in the configuration file. The keys are converted to uppercase and dots are replaced with underscores.

Example: `db.url` becomes `DB_URL`.

### Host Override
The database host can be overridden globally using the system property `db.hostOverride` or the environment variable `STROLCH_DB_HOST_OVERRIDE` (if `db.allowHostOverrideEnv` is true). This is particularly useful in containerized environments.

## Archive Support
The module also supports a separate archive database via `PostgreSqlDataArchiveHandler`. This allows moving old data to a separate database to keep the main database performant.
