### Strolch Realms

Strolch Realms provide a mechanism for mandate separation (multi-tenancy) within a single Strolch Agent. Each realm has its own data maps (Resources, Orders, Activities) and can be configured independently.

#### RealmHandler

The `RealmHandler` is a `StrolchComponent` that manages the lifecycle of all configured realms. It provides access to the realms by name.

To access the `RealmHandler` from another component:

```java
RealmHandler realmHandler = getContainer().getComponent(RealmHandler.class);
StrolchRealm realm = realmHandler.getRealm(StrolchConstants.DEFAULT_REALM);
```

#### StrolchRealm

A `StrolchRealm` represents a single mandate. It provides methods to:
- Open transactions (`openTx`).
- Lock and unlock elements by `Locator`.
- Access the `DataStoreMode`.
- Check configuration settings like audit trail, versioning, etc.
- Access the `ObserverHandler` (if enabled).

#### Data Store Modes

The `DataStoreMode` defines how data is managed and persisted for a realm:

- **EMPTY**: A transient realm that starts empty and does not persist any data.
- **TRANSIENT**: A transient realm that is populated during initialization from an XML file (configured via `dataStoreFile`) but does not persist changes.
- **CACHED**: Data is kept in memory (cached) and all changes are persisted via a `PersistenceHandler` (e.g., PostgreSQL). **Note**: This mode requires a [PersistenceHandler](#persistencehandler) to be configured in the component configuration.
- **ECLIPSE_STORE**: Data is managed using EclipseStore, providing high-performance persistence without a traditional database. **Note**: This mode is still experimental.

#### ObserverHandler

The `ObserverHandler` implements the observer pattern for Strolch elements. It allows components or external systems to be notified when Resources, Orders, or Activities are added, updated, or removed.

If `enableObserverUpdates` is set to `true` (the default), the `ObserverHandler` can be accessed from the realm:

```java
ObserverHandler observerHandler = realm.getObserverHandler();
observerHandler.registerObserver(Tags.RESOURCE, myObserver);
```

Observers must implement the `Observer` interface. Notifications are delivered asynchronously to avoid blocking transactions.

#### Configuration

Realms are configured within the `RealmHandler` component in the `StrolchConfiguration.xml` file.

##### Basic Configuration

The `realms` property defines the list of realms to be initialized. The default is `defaultRealm`.

```xml
<Component>
    <name>RealmHandler</name>
    <api>li.strolch.agent.api.RealmHandler</api>
    <impl>li.strolch.agent.impl.DefaultRealmHandler</impl>
    <Properties>
        <realms>defaultRealm,myOtherRealm</realms>
        <dataStoreMode>CACHED</dataStoreMode>
        <dataStoreMode.myOtherRealm>TRANSIENT</dataStoreMode.myOtherRealm>
        <dataStoreFile.myOtherRealm>StrolchModel.xml</dataStoreFile.myOtherRealm>
    </Properties>
</Component>
```

##### Realm-specific Properties

Most properties can be configured per realm by appending the realm name to the property key (except for `defaultRealm`, which uses the base key).

| Property | Description | Default |
| --- | --- | --- |
| `dataStoreMode` | The `DataStoreMode` for the realm. | (Required) |
| `dataStoreFile` | Relative path to the XML model file. Required for `TRANSIENT` and optional for `ECLIPSE_STORE`. | - |
| `enableAuditTrail` | Enables the audit trail for this realm. | `false` |
| `enableModelAudits` | Enables auditing of model changes (requires `enableAuditTrail`). | `false` |
| `enableAuditsOnRead` | Enables auditing of model reads (requires `enableModelAudits`). | `false` |
| `enableAuditsForAudits` | Enables auditing of audit trail access. | `false` |
| `enableAuditsForSystemUsers` | Enables auditing for system users (e.g., `agent`). | `false` |
| `enableObserverUpdates` | Enables observer updates for this realm. | `true` |
| `enableVersioning` | Enables versioning of Resources, Orders, and Activities. | `false` |
| `txLoggingThresholdMs` | Threshold in milliseconds for logging successful transactions. | `0` (all logged) |
| `tryLockTime` | Time to wait for a lock before failing. | `10` |
| `tryLockTimeUnit` | Unit for `tryLockTime` (e.g., `SECONDS`, `MILLISECONDS`). | `SECONDS` |

##### EclipseStore Specific Properties

When using `dataStoreMode=ECLIPSE_STORE`, the following additional properties are available:

| Property | Description | Default |
| --- | --- | --- |
| `dbStore` | Relative path to the directory where data is stored. | (Required) |
| `enableBackup` | Enables backup for the EclipseStore. | `false` |
| `allowDataInitOnSchemaCreate` | Allows initializing the realm from an XML file if the store is empty. | `false` |
| `dataStoreFile` | Relative path to the XML model file (required if `allowDataInitOnSchemaCreate=true`). | - |
 
#### PersistenceHandler

The `PersistenceHandler` is responsible for persisting data when using the `CACHED` data store mode. Strolch provides an implementation for PostgreSQL.

##### PostgreSQL Configuration Example

The following example shows how to configure the `PersistenceHandler` for a PostgreSQL database:

```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.persistence.api.PersistenceHandler</api>
    <impl>li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler</impl>
    <Properties>
        <allowDataInitOnSchemaCreate>true</allowDataInitOnSchemaCreate>
        <allowSchemaCreation>true</allowSchemaCreation>
        <allowSchemaMigration>true</allowSchemaMigration>
        <allowSchemaDrop>true</allowSchemaDrop>
        <db.url>jdbc:postgresql://localhost/esyboxtest</db.url>
        <db.username>esyboxtest</db.username>
        <db.password>esyboxtest</db.password>
        <db.pool.maximumPoolSize>5</db.pool.maximumPoolSize>
        <db.pool.keepaliveTime>600000</db.pool.keepaliveTime>
    </Properties>
</Component>
```

Note that the `RealmHandler` (or specifically the realm using `CACHED` mode) must depend on the `PersistenceHandler`:

```xml
<Component>
    <name>RealmHandler</name>
    <depends>PersistenceHandler</depends>
    ...
</Component>
```

##### Configuration Example

```xml
<Component>
    <name>RealmHandler</name>
    <api>li.strolch.agent.api.RealmHandler</api>
    <impl>li.strolch.agent.impl.DefaultRealmHandler</impl>
    <Properties>
        <!-- Define two realms -->
        <realms>defaultRealm,testRealm</realms>

        <!-- Configuration for defaultRealm -->
        <dataStoreMode>CACHED</dataStoreMode>
        <enableAuditTrail>true</enableAuditTrail>
        <enableModelAudits>true</enableModelAudits>
        <enableVersioning>true</enableVersioning>

        <!-- Configuration for testRealm -->
        <dataStoreMode.testRealm>TRANSIENT</dataStoreMode.testRealm>
        <enableAuditTrail.testRealm>false</enableAuditTrail.testRealm>
        <enableObserverUpdates.testRealm>false</enableObserverUpdates.testRealm>
    </Properties>
</Component>
```
