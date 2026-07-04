# XML Persistence Technical Specification

## Overview
The `strolch-persistence-xml` module provides a filesystem-based XML persistence implementation for the Strolch framework. It stores Strolch elements (Resources, Orders, Activities, Audits, and LogMessages) as individual XML files on the local disk.

This implementation is often used as a reference or for small applications where a full database like PostgreSQL is not required.

## Architecture

### Persistence Handler
The central component is `XmlPersistenceHandler`, which implements the `PersistenceHandler` interface. It is responsible for:
- Initializing the `PersistenceManager` (from the `strolch-xmlpers` library) for each realm.
- Configuring the base path for storage.
- Opening transactions.
- Providing DAOs for Strolch elements.

### Transactions
`XmlStrolchTransaction` wraps an `xmlpers` `PersistenceTransaction`. It manages the lifecycle of the transaction and ensures that all operations are performed within the context of the underlying filesystem storage.

### Data Access Objects (DAOs)
Each Strolch element type has a corresponding DAO implementation:
- `XmlResourceDao`
- `XmlOrderDao`
- `XmlActivityDao`
- `XmlAuditDao`
- `XmlLogMessageDao`

These DAOs use `xmlpers` to perform CRUD operations on the XML files.

### Limitations
- **Paging**: Paging is not supported by this persistence handler.
- **Versioning**: Versioning of elements is not supported.
- **Performance**: As every element is stored in a separate file, performance may degrade with a very large number of elements compared to a relational database.

## Storage Format
Data is stored in a directory structure managed by the `strolch-xmlpers` library. Each realm has its own subdirectory. Within a realm, elements are organized by type and then by ID.

Example structure:
```
dbStore/
└── default/
    ├── Resource/
    │   ├── MyType/
    │   │   ├── element1.xml
    │   │   └── element2.xml
    │   └── AnotherType/
    │       └── element3.xml
    ├── Order/
    │   └── ...
    └── Audit/
        └── ...
```

Elements are parsed using SAX for high performance.

## Configuration

The `XmlPersistenceHandler` is configured in the `StrolchConfiguration.xml` file.

### Component Configuration
```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.persistence.api.StrolchPersistenceHandler</api>
    <impl>li.strolch.persistence.xml.XmlPersistenceHandler</impl>
    <Properties>
        <dbStorePath>dbStore</dbStorePath>
        <allowDataInitOnEmptyDb>true</allowDataInitOnEmptyDb>
        <verbose>false</verbose>
    </Properties>
</Component>
```

### Properties Reference

| Property | Description | Default |
| --- | --- | --- |
| `dbStorePath` | Relative path from the data directory to the storage root. | **Required** |
| `allowDataInitOnEmptyDb` | If true, initializes the DB from the realm's data file if the storage is empty. | `false` |
| `verbose` | Enables verbose logging for the underlying storage manager. | `false` |
| `ignoreRealm` | If true, the persistence handler will ignore this realm. | `false` |
| `db.useEnv` | If true, configuration values are read from environment variables. | `false` |

### Multi-Realm Configuration
Realms can have their own storage path by appending the realm name to the property key:
```xml
<dbStorePath.myrealm>dbStore_myrealm</dbStorePath.myrealm>
```

### Environment Variables
If `db.useEnv` is set to `true`, the following environment variables are used (replace `<realm>` with the realm name, e.g., `DEFAULT`):
- `DBSTOREPATH_<realm>`
- `VERBOSE_<realm>`

Note: The environment variable keys are formed using `makeRealmKey(realmName, key, true)` which converts the key to uppercase and replaces dots with underscores.
