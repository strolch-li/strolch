# Core Framework: Strolch

[Strolch](https://strolch.li) is a Resource-Order-Activity based framework.

- **Resources**: Represent entities (e.g., `Person`, `Product`).
- **Orders**: Represent entities requiring a state and timestamp (e.g. a `FromStock` order with positions and quantities to be dispensed by a specific time).
- **Activities**: Represent entities with a hierarchical structure actionable tasks, (e.g. `FromStock` to plan/control an activity of dispensing a product).
- **Transactions**: Use `StrolchTransaction` for data access and modification.

### Strolch Object Model

The project uses the Strolch framework, which is based on **Resources**, **Orders**, and **Activities**. These elements
share a common hierarchy for managing data through **ParameterBags** and **Parameters**.

#### Element Hierarchy

- `Resource`, `Order`, `Activity` inherit from `AbstractStrolchRootElement`.
- `Action` (used within Activities) inherits from `GroupedParameterizedElement`.
- Both `AbstractStrolchRootElement` and `GroupedParameterizedElement` implement the `ParameterBagContainer` interface.

#### Relationships

Relationships between root elements are defined using parameters, typically stored in a `ParameterBag` with the ID `relations`.

- **1-to-1**: Represented by a `StringParameter` where the value is the ID of the target element.
- **1-to-N**: Represented by a `StringListParameter` where the values are the IDs of the target elements.

**Metadata for Relationships:**
To allow tools and the UI to understand the nature of a relationship, the following metadata should be set:
- **Interpretation**: Indicates the type of root element being referenced. Use the constants:
	- `INTERPRETATION_RESOURCE_REF` ("Resource-Ref")
	- `INTERPRETATION_ORDER_REF` ("Order-Ref")
	- `INTERPRETATION_ACTIVITY_REF` ("Activity-Ref")
- **Unit of Measure (UOM)**: Stores the `type` of the referenced element (e.g., "Product", "User").

**XML Representation of Relationships:**
In Strolch XML (e.g., `templates.xml` or `Model.xml`), relationships are defined as `Parameter` elements within a `ParameterBag` that has the ID `relations`. The metadata is set using the `Interpretation` and `Uom` attributes.

```xml
<ParameterBag Id="relations" Name="Relations" Type="Relations">
    <!-- 1-to-1 relationship to a Resource of type 'Location' -->
    <Parameter Id="location" Name="Location" Type="String" Interpretation="Resource-Ref" Uom="Location" Value=""/>
    
    <!-- 1-to-N relationship to Resources of type 'Part' -->
    <Parameter Id="parts" Name="Parts" Type="StringList" Interpretation="Resource-Ref" Uom="Part" Value=""/>
</ParameterBag>
```

Note: All attribute names e.g. `Id`, `Name`, `Interpretation`, `Uom`, etc. are **case-sensitive** and must be capitalized as shown.

**Convenience Methods:**
Root elements provide convenience methods for common relationship operations between any `StrolchRootElement` (Resource, Order, or Activity):
```java
// Set a 1-to-1 relation to a Resource
resource.setRelation("product", productResource);

// Set a 1-to-1 relation to an Order
resource.setRelation("currentOrder", order);

// Set a 1-to-1 relation using only the ID
resource.setRelationId("parent", "otherElementId");

// Add a Resource to a 1-to-N relation
resource.addRelation("parts", partResource);

// Retrieve the ID of a related element
String productId = resource.getRelationId("product");

// Retrieve all IDs of a 1-to-N relation
List<String> partIds = resource.getRelationIds("parts");

// When retrieving the related element itself, prefer using the StrolchTransaction convenience method:
// Resource related = tx.getResourceByRelation(resource, "product", true);
```
The `relationsBag()` method provides access to the dedicated bag, creating it if it doesn't exist.

#### Convenience Methods for Parameters

Instead of manually retrieving a `ParameterBag` and then a `Parameter` object, use the convenience methods provided by
`ParameterBagContainer`. These methods simplify code and handle the default parameter bag (`parameters`) automatically.

**1. Retrieving Values directly (preferred)**
Use these when you just need the value. They handle null checks (returning default values for primitives) or throwing
exceptions if the parameter is missing (if `assertExists` is used internally).

```java
// Good: Direct value retrieval from default bag
String updatedBy = element.getString(PARAM_UPDATED_BY);
int version = element.getInteger(PARAM_VERSION);
double weight = element.getDouble(PARAM_WEIGHT);
boolean active = element.getBoolean(PARAM_ACTIVE);
ZonedDateTime date = element.getDate(PARAM_DATE);

// Bad: Manual retrieval
StringParameter updatedByP = element.getParameter(BAG_PARAMETERS, PARAM_UPDATED_BY);
String updatedBy = updatedByP.getValue();
```

**2. Accessing Parameters in specific bags**
If the parameter is not in the default `parameters` bag, you can still use convenience methods by providing the bag ID.

```java
// Good: Specifying the bag
String value = element.getString(BAG_CUSTOM, PARAM_MY_PARAM);

// Instead of:
ParameterBag bag = element.getParameterBag(BAG_CUSTOM);
String value = bag.getString(PARAM_MY_PARAM);
```

**3. Retrieving Parameter Objects**
If you need the `Parameter` object itself (e.g., to check its metadata or pass it to a method), use the methods with the
`P` suffix.

```java
StringParameter updatedByP = element.getStringP(PARAM_UPDATED_BY);
IntegerParameter versionP = element.getIntegerP(PARAM_VERSION);
```

**4. Setting Values**
Convenience methods also exist for setting values, which will create the parameter if it doesn't exist (in the default
bag).

```java
element.setString(PARAM_UPDATED_BY, "admin");
element.setInteger(PARAM_VERSION, 2);
```

**5. Hierarchical Lookup (Activities/Actions)**
For `Activity` and `Action` elements, you can use `findParameter(paramKey)` to search for a parameter up the activity
hierarchy if it's not found on the current element.

```java
// Searches on current element, then parent activity, and so on
String theme = action.findParameter(PARAM_THEME).getValue();
```

#### Timed States

Timed states are owned by `Resource` elements and represent values whose history or schedule is part of the model (e.g., availability, stock).

Resources provide lookup and filtering methods:
- `getTimedState(String id);`
- `hasTimedState(String id);`
- `streamOfTimedStates();`
- `streamOfTimedStatesByInterpretation(String interpretation);`

#### Builders

The `li.strolch.model.builder` package provides a fluent API for constructing Strolch elements programmatically, useful for creating test data or default model fragments.

```java
Resource resource = new ResourceBuilder("product01", "My Product", "Product")
    .bag("parameters", "Parameters")
        .string("color", "Color").value("Red").end()
        .integer("weight", "Weight").value(10).end()
    .endBag()
    .floatState("stock", "Stock").end()
    .build();

// Relationship shorthands in builders
Resource resource = new ResourceBuilder("res01", "Resource 1", "Type1")
    .resourceRelation("parent", "ParentResource") // 1-to-1 to a Resource of type "ParentResource"
    .orderRelations("tasks", "OrderType")         // 1-to-N to Orders of type "OrderType"
    .build();
```

#### JSON Serialization

Strolch supports **Full Serialization** (preserving all metadata) and **Flat Serialization** (mapping parameter values directly to key-value pairs). Flat serialization is preferred for REST APIs.

```java
// Serialize to flat JSON
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor().flat());

// Read flat JSON back into a Strolch element
resource.accept(new FromFlatJsonVisitor(json));
```

#### Exceptions

Strolch uses a hierarchy of runtime exceptions derived from `StrolchException`:
- `StrolchModelException`: Model-specific errors (e.g., accessing non-existent elements).
- `StrolchElementNotFoundException`: Requested element does not exist.
- `StrolchAccessDeniedException`: Privilege check failed.
- `StrolchUserMessageException`: Error message intended for display to the user.

### Strolch Services, Commands, Searches and Policies

Business logic in Strolch is encapsulated in Services and Commands. Data retrieval is done via Searches, and extensible
behavior is implemented through Policies.

#### Strolch Transactions

Transactions (`StrolchTransaction`) are the primary way to interact with the data model. They handle locking, auditing, and ensure data consistency.

**1. Opening a Transaction**
Always use try-with-resources. Writeable transactions should use `rollbackOnFailure()` and must call `commitOnClose()`.

```java
try (StrolchTransaction tx = agent.openTx(cert, "MyAction", false).rollbackOnFailure()) {
    // perform operations
    tx.commitOnClose();
}
```

**2. Retrieving Elements**
Use the convenience methods on `StrolchTransaction` instead of accessing maps directly.

```java
// Retrieve by type and ID
Resource resource = tx.getResourceBy("Product", "p01", true);
Order order = tx.getOrderBy("OrderType", "o01", true);

// Retrieve using a relation parameter (preferred)
Resource related = tx.getResourceByRelation(resource, "product", true);

// Retrieve using a relation parameter object
Resource related = tx.getResourceBy(resource.getStringP("product"), true);

// Find any element by locator
StrolchElement element = tx.findElement(Locator.valueOf("Resource/Product/p01"));
```

**3. Modifying Elements**
Always lock elements before modification. `readLock()` is recommended as it retrieves a fresh copy under lock.

```java
// Lock and get fresh copy
Resource resource = tx.readLock(resource);
resource.setString("color", "blue");
tx.update(resource);
```

#### Services

Services are the entry point for business logic. They are typically called from REST resources or other high-level
components.

- Inherit from `AbstractService<T, U>`.
- Implement `internalDoService(T arg)`.
- Use `openArgOrUserTx(arg)` to manage transactions.
- Always use try-with-resources for transactions.

**Authorization:**
Strolch's runtime automatically performs privilege assertions for services when they are invoked via the `DefaultServiceHandler`. 
You do **not** need to call `tx.assertHasPrivilege(getClass().getName())` inside the service. 
Instead, you must define the allowed services for each role in `PrivilegeRoles.xml`:

```xml
<Role name="Employee">
    <Privilege name="li.strolch.service.api.Service" policy="DefaultPrivilege">
        <Allow>ch.atexxi.chronivaro.core.service.StartTimerService</Allow>
    </Allow>
</Role>
```

```java
public class RemoveGatewayService extends AbstractService<StringArgument, ServiceResult> {
	@Override
	protected ServiceResult internalDoService(StringArgument arg) throws Exception {
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			Resource gateway = tx.getResourceBy(TYPE_GATEWAY, arg.value, true);
			tx.assertHasPrivilege(Operation.REMOVE, gateway); // Use for data-level checks

			// perform logic or call commands
			tx.remove(gateway);

			tx.commitOnClose();
		}
		return ServiceResult.success();
	}
}
```

#### Commands

Commands are used within a transaction to perform a specific, reusable atomic operation.

- Inherit from `Command`.
- Implement `validate()` for pre-conditions.
- Implement `doCommand()` for the actual logic.
- Access the transaction via `tx()`.

```java
public class MyCommand extends Command {
	public MyCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Resource must be set", this.resource);
	}

	@Override
	public void doCommand() {
		tx().addOrUpdate(this.resource);
	}
}
```

#### Querying / Searches

Searches provide a fluent API for querying Strolch elements. Use `ResourceSearch`, `OrderSearch`, or `ActivitySearch`.

**Authorization:**
When a query for objects is performed using a `StrolchSearch`, the Strolch runtime automatically guarantees that a privilege assertion is performed for the user. 
Similar to services, ensure the relevant search privileges are defined in `PrivilegeRoles.xml`.

**Filtering and Logical Operators:**
```java
List<Resource> results = new ResourceSearch()
    .types("Product")
    .where(id(isEqualTo("p01"))
        .and(param("parameters", "color", isEqualTo("red"))))
    .search(tx)
    .toList();
```

**Navigation through Relations:**
You can navigate through relations between elements using `relationName` or `relationParam`.
```java
// Filter by the name of a related element
new ResourceSearch().types("Slot")
    .where(relationName(tx, "location", isEqualTo("Warehouse-A")))
    .search(tx).toList();

// Filter by a parameter of a related element
new ResourceSearch().types("Slot")
    .where(relationParam(tx, "location", "color", isEqualTo("yellow")))
    .search(tx).toList();
```

**Processing Results:**
The `search(tx)` method returns a `SearchResult` with many terminal operations:
- `toList()`, `toSet()`, `toSingleton()`, `toSingletonO()`.
- `toJsonArray(jsonMapper)`, `toPaging(offset, limit)`.
- `forEach(consumer)`, `isEmpty()`, `isNotEmpty()`.
- `orderById(reversed)`, `orderByName(reversed)`, `orderByParam(bagId, paramId, reversed)`.

#### Policies

Policies allow for extensible and interchangeable logic defined in XML configuration.

- Inherit from `StrolchPolicy`.
- Accessed via `tx.getPolicy(Class<T> policyClass, PolicyDef policyDef)`.

```java
public abstract class MyPolicy extends StrolchPolicy {
	public MyPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public abstract void execute();
}

// In code:
PolicyDef policyDef = element.getPolicyDef("MyPolicy");
MyPolicy policy = tx().getPolicy(MyPolicy.class, policyDef);
policy.execute();
```

### Entity Model Migrations (MigrationsHandler, CodeMigration, DataMigration)

As application domain models evolve over time, existing data in Strolch realms must be migrated safely. Strolch provides the `MigrationsHandler` component (`li.strolch.migrations.MigrationsHandler`) to manage and execute both file-based data migrations and programmatic code migrations across realms.

#### Version Tracking

Strolch tracks migration progress per realm using a dedicated `Resource`:
- **Locator / Identity**: `Resource/Migrations/migrations`
- **Parameters Bag (`parameters`)**:
	- `currentDataVersion`: `StringParameter` representing the current version of applied XML data migrations.
	- `currentCodeVersion`: `StringParameter` representing the current version of applied Java code migrations.
- **Versioning**: Uses `li.strolch.utils.Version` (e.g., `0.7.0`, `1.2.0`). Migrations are executed in ascending version order, and only migrations with a version strictly greater than the realm's current version are executed.

#### Migration Types

1. **Data Migrations (`DataMigration`)**:
	- File-based XML model imports located at `runtime/data/migrations/data/<realmName>/<version>.xml`.
	- Automatically imported using `XmlImportModelCommand` to add or update resources and orders.
	- Updates `currentDataVersion` on completion.

2. **Code Migrations (`CodeMigration`)**:
	- Programmatic migrations in Java extending `li.strolch.migrations.CodeMigration`.
	- Used for complex model evolution: renaming/restructuring parameters, migrating relations, updating timed states, removing deprecated fields, or performing calculations/backfills across existing entities.
	- Updates `currentCodeVersion` on completion.

#### Implementing a Code Migration

When updating entity models in an existing application:

1. **Define Version Constants**:
   Centralize migration versions in a constants class per domain/subsystem.
   ```java
   public class CustomMigrations {
   	public static final Version version_0_24_0 = Version.valueOf("0.24.0");
   	public static final Version version_0_25_0 = Version.valueOf("0.25.0");
   }
   ```

2. **Extend `CodeMigration`**:
   Implement the migration by querying entities, performing transformations, and committing changes.
   ```java
   public class MigrateProductStorageAfterOpeningMigration extends CodeMigration {

   	public MigrateProductStorageAfterOpeningMigration(String realm) {
   		super(realm, CustomMigrations.version_0_25_0);
   	}

   	@Override
   	public void migrate(ComponentContainer container, Certificate certificate) {
   		int[] productsUpdated = new int[1];
   		try (StrolchTransaction tx = openTx(container, certificate)) {
   			tx.setSuppressUpdates(true);

   			tx.streamResources(TYPE_PRODUCT).forEach(product -> {
   				try {
   					product = product.ensureModifiable();
   					migrateProduct(product);
   					tx.update(product);
   					productsUpdated[0]++;
   				} catch (IllegalStateException e) {
   					logger.error("[{}] Failed to migrate product {} {}", getRealm(),
   							product.getId(), product.getName(), e);
   				}
   			});

   			buildMigrationVersionChangeCommand(tx);
   			tx.commitOnClose();
   		}

   		logger.info("[{}] Migrated {} products", getRealm(), productsUpdated[0]);
   	}

   	// Package-private for isolated unit testing
   	void migrateProduct(Resource product) {
   		if (product.hasParameter(PARAM_OBSOLETE_IS_RELOCATION_WARM_2_COLD)) {
   			boolean warmToCold = product.is(PARAM_OBSOLETE_IS_RELOCATION_WARM_2_COLD);
   			if (warmToCold)
   				product.setString(PARAM_RELOCATION_TYPE, RelocationType.WarmToCold);
   			product.removeParameter(PARAM_OBSOLETE_IS_RELOCATION_WARM_2_COLD);
   		}
   	}
   }
   ```

**Important Guidelines for Code Migrations:**
- **Always update the migration version**: Call `buildMigrationVersionChangeCommand(tx);` inside the transaction before `tx.commitOnClose()`.
- **Suppress updates when batching**: Use `tx.setSuppressUpdates(true)` to avoid broadcasting intermediate change events during bulk updates.
- **Ensure modifiability**: Always invoke `element.ensureModifiable()` or `tx.readLock(element)` before mutating elements.
- **Extract transformation logic**: Extract entity transformation into helper methods (e.g., `migrateProduct(Resource)`) to enable isolated unit testing.

#### Executing Migrations in Applications

In applications code migrations are typically triggered on application boot via a `StrolchJob`:

1. **Create the Migration Job**:
   ```java
   public class RunCodeMigrationsJobs extends StrolchJob {

   	public RunCodeMigrationsJobs(StrolchAgent agent, String id, String name, JobMode jobMode) {
   		super(agent, id, name, jobMode);
   	}

   	@Override
   	protected void execute(PrivilegeContext ctx) {
   		logger.info("Running code migrations...");
   		MigrationsHandler migrationsHandler = getContainer().getComponent(MigrationsHandler.class);

   		for (String realm : getContainer().getRealmNames()) {
   			MapOfLists<String, CodeMigration> codeMigrationsByRealm = new MapOfLists<>();
   			codeMigrationsByRealm.addElement(realm, new MigrateDivisibleOnProductMigration(realm));
   			codeMigrationsByRealm.addElement(realm, new ProductStockMigration(realm));
   			codeMigrationsByRealm.addElement(realm, new MigrateProductStorageAfterOpeningMigration(realm));

   			migrationsHandler.runCodeMigrations(ctx.getCertificate(), codeMigrationsByRealm);
   		}

   		logger.info("Code migrations completed.");
   	}
   }
   ```

2. **Trigger on Application Startup**:
   In your `PostInitializer` (e.g., `EsyBoxPostInitializer.java`):
   ```java
   StrolchJobsHandler jobsHandler = getComponent(StrolchJobsHandler.class);
   jobsHandler.registerLenient(RunCodeMigrationsJobs.class).runNow();
   ```

3. **Configure in `StrolchConfiguration.xml`**:
   ```xml
   <Component>
       <name>MigrationsHandler</name>
       <api>li.strolch.migrations.MigrationsHandler</api>
       <impl>li.strolch.migrations.MigrationsHandler</impl>
       <depends>RealmHandler</depends>
       <Properties>
           <runMigrationsOnStart>false</runMigrationsOnStart>
           <verbose>false</verbose>
       </Properties>
   </Component>
   ```

#### Unit Testing Migrations

Unit test the transformation logic in isolation by constructing legacy elements and verifying the migrated state:

```java
public class MigrateProductStorageAfterOpeningMigrationTest {

	private final MigrateProductStorageAfterOpeningMigration migration
			= new MigrateProductStorageAfterOpeningMigration("test");

	@Test
	public void shouldMigrateLegacyRelocationValues() {
		Resource product = Product.newProduct();
		product.setBoolean(PARAM_OBSOLETE_IS_RELOCATION_WARM_2_COLD, true);

		this.migration.migrateProduct(product);

		assertEquals(RelocationType.WarmToCold.name(), product.getString(PARAM_RELOCATION_TYPE));
		assertFalse(product.hasParameter(PARAM_OBSOLETE_IS_RELOCATION_WARM_2_COLD));
	}
}
```

### Generic Strolch Specification

When building Strolch based applications, it is important to follow certain guidelines and best practices to ensure
consistency, maintainability, and readability. These guidelines are specified in the following document:
[Strolch Specification for building applications](STROLCH_SPECIFICATION.md)
