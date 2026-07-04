# Migrations

The Strolch Migrations framework provides a structured way to handle data and code changes as the application evolves. It ensures that the Strolch model and associated logic are updated correctly when moving between versions.

## Core Concepts

### MigrationsHandler

The `MigrationsHandler` is a Strolch component that manages the discovery and execution of migrations. It can be configured to run migrations automatically during system startup or on demand.

### Migration Types

There are two main types of migrations:

1.  **Data Migration**: Focuses on updating the Strolch model (Resources, Orders, Activities). These usually involve importing XML files that represent the changes.
2.  **Code Migration**: Involves running Java code to perform more complex updates that cannot be easily expressed in XML.

### Migration Versioning

The framework tracks the current version of the data and code in a special `Resource` of type `Migrations` and ID `migrations`. This resource stores the current versions as `StringParameter` in the `parameters` bag:
- `currentDataVersion`: The version of the last executed data migration.
- `currentCodeVersion`: The version of the last executed code migration.

## How it Works

1.  **Discovery**: On startup, the `MigrationsHandler` looks for migration files in the component's data directory: `{dataDir}/li.strolch.migrations.MigrationsHandler/migrations`. It expects two subdirectories: `data` and `code`.
2.  **Organization**: Inside `data` and `code`, migrations are organized by realm (e.g., `migrations/data/my_realm/`).
3.  **Version Check**: It compares the versions of the available migration files with the current version stored in the Strolch model. The framework tracks data and code versions separately.
4.  **Execution**: It executes the missing migrations in the correct order (sorted by version).
5.  **Update**: After each successful migration, the version in the model is updated.

## Creating a Migration

### Data Migration

Data migrations are XML files containing Strolch elements (Resources, Orders, Activities) to be added or updated. The filename must be the version number followed by `.xml` (e.g., `1.0.0.xml`).

Place these files in: `migrations/data/{realm}/{version}.xml`.

### Code Migration

Code migrations allow for complex logic. You can either:
1.  Provide a marker XML file in `migrations/code/{realm}/{version}.xml` and then programmatically pass `CodeMigration` implementations to the `MigrationsHandler`.
2.  Subclass `li.strolch.migrations.CodeMigration` and override the `migrate` method.

```java
public class MyCodeMigration extends CodeMigration {
    public MyCodeMigration(String realm, Version version) {
        super(realm, version);
    }

    @Override
    public void migrate(ComponentContainer container, Certificate certificate) {
        try (StrolchTransaction tx = openTx(container, certificate)) {
            // Perform custom migration logic
            tx.commitOnClose();
        }
    }
}
```

## Configuration

The `MigrationsHandler` is configured as a Strolch component in `strolch.xml`:

```xml
<Component>
  <name>MigrationsHandler</name>
  <api>li.strolch.migrations.MigrationsHandler</api>
  <impl>li.strolch.migrations.MigrationsHandler</impl>
  <Properties>
    <runMigrationsOnStart>true</runMigrationsOnStart>
    <pollMigrations>false</pollMigrations>
    <pollWait>5</pollWait>
    <verbose>false</verbose>
  </Properties>
</Component>
```

| Property | Description | Default |
| --- | --- | --- |
| `runMigrationsOnStart` | Whether to run migrations during component startup. | `false` |
| `pollMigrations` | Whether to periodically poll the migrations directory for new files. | `false` |
| `pollWait` | The polling interval in minutes. | `5` |
| `verbose` | Whether to log detailed information about found and executed migrations. | `false` |
