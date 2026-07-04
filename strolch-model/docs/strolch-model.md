Strolch Model Technical Specification
=====================================

Purpose
-------

The `strolch-model` module defines the in-memory data model for Strolch. It contains no persistence implementation and no agent
runtime lifecycle. Its responsibility is to provide the shared Java types and serialization contracts used by higher-level modules.

Module boundaries
-----------------

The module provides:

* root element classes: `Resource`, `Order`, and `Activity`;
* activity element classes such as `Action` and value changes;
* parameter containers, parameter bags, and typed parameter implementations;
* timed state implementations for values over time;
* policy definition containers;
* locator support for addressing elements inside a model;
* audit records for tracking element access and changes;
* log messages for system events and i18n support;
* visitor APIs for traversal and conversion;
* builders for constructing model elements;
* XML and JSON conversion helpers;
* the XML schema `StrolchModel.xsd`.

The module does not provide:

* transaction management;
* persistence backends;
* privilege checks;
* REST endpoints;
* business services or commands.

Element hierarchy
-----------------

The central hierarchy is:

* `StrolchElement` is the common interface for model elements.
* `AbstractStrolchElement` provides common element metadata and read-only handling.
* `AbstractStrolchRootElement` is the base class for root elements with parameter bags and policy definitions.
* `Resource`, `Order`, and `Activity` are root elements.
* `GroupedParameterizedElement` is the base class for parameterized child elements.
* `Action` is a child element used inside activities.

Root elements are globally addressable by their type and ID in persistence modules. Child elements are addressed through their parent
hierarchy and locators.

Parameters
----------

Any `ParameterBagContainer` can contain multiple `ParameterBag` instances. The default bag is conventionally named `parameters`.
Consumers should use the typed convenience methods on `ParameterBagContainer` for common access patterns:

```java
boolean enabled = element.getBoolean("enabled");
boolean disabled = element.is("disabled");
int version = element.getInteger("version");
String externalId = element.getString("externalId");
element.setString("updatedBy", "system");
```

Typed parameter classes keep the string representation used by XML separate from the Java value representation. List parameters are
represented by dedicated list parameter classes and should be used instead of serializing custom delimiters into a string parameter.

Timed states
------------

Timed states are owned by `Resource` elements. A timed state has an ID, name, type, optional interpretation, optional unit of measure,
and a set of timestamped values. Use timed states for values whose history or schedule is part of the model, for example availability,
stock, measured values, or planned state changes.

Resources provide lookup and filtering methods such as:

* `getTimedState(String id);`
* `hasTimedState(String id);`
* `streamOfTimedStates();`
* `streamOfTimedStatesByInterpretation(String interpretation);`
* `streamOfTimedStatesByInterpretationAndUom(String interpretation, String uom);`

Policies
--------

Policy definitions are stored as a mapping from policy type to implementation value. The model only stores the definitions. Runtime
modules are responsible for resolving a `PolicyDef` to an executable policy implementation.

Policy definitions should be used when behavior must remain configurable in model data rather than hard-coded in services or commands.

Relationships
-------------

Relationships between root elements are defined using parameters. By convention, these parameters are stored in a
`ParameterBag` with the ID `relations`.

### Relationship types

* **1-to-1**: Represented by a `StringParameter` where the value is the ID of the target element.
* **1-to-N**: Represented by a `StringListParameter` where the values are the IDs of the target elements.

### Metadata

To allow tools and the UI to understand the nature of a relationship, the following metadata should be set:

* **Interpretation**: Indicates the type of root element being referenced. Use the constants:
    - `INTERPRETATION_RESOURCE_REF` ("Resource-Ref")
    - `INTERPRETATION_ORDER_REF` ("Order-Ref")
    - `INTERPRETATION_ACTIVITY_REF` ("Activity-Ref")
* **Unit of Measure (UOM)**: Stores the `type` of the referenced element (e.g., "Product", "User").

### Convenience methods

Root elements provide convenience methods for common relationship operations:

```java
// Set a 1-to-1 relation to a Resource
resource.setRelation("parent", otherResource);

// Add a Resource to a 1-to-N relation
resource.addRelation("parts", partResource);

// Retrieve the ID of a related element
String parentId = resource.getRelationId("parent");

// Retrieve all IDs of a 1-to-N relation
List<String> partIds = resource.getRelationIds("parts");
```

The `relationsBag()` method provides access to the dedicated bag, creating it if it doesn't exist.

### Specialized relationships

Beyond the general `relations` bag convention, some model elements have specialized relationship fields:

* `Action` elements reference a target `Resource` using dedicated `resourceId` and `resourceType` fields.

Activities
----------

Activities represent structured work. An activity has a `TimeOrdering` value:

* `Series` means children are ordered sequentially.
* `Parallel` means children may be considered in parallel.

Activities can contain other activities and actions. Actions may reference target resources through `ResourceId` and `ResourceType`.
Actions can also contain value changes that describe timed state changes.

Hierarchical parameter lookup allows activity elements to reuse context from parent activities. Use `findParameter(paramId)` where a
child element should inherit a value from its activity hierarchy.

Audits
------

Audits record access and changes to elements, providing a history of "who did what and when".

### Audit fields

* **id**: Unique identifier for the audit record.
* **username**: The user who performed the action.
* **date**: When the action occurred.
* **elementType**: The type of element accessed (e.g., "Resource").
* **elementSubType**: The sub-type of the element (e.g., "Product").
* **elementAccessed**: The ID of the element accessed.
* **newVersion**: Optional timestamp of the new version created by the action.
* **action**: The specific action performed (e.g., "Update").
* **accessType**: The type of access (`READ`, `CREATE`, `UPDATE`, `DELETE`, `EXECUTE`).
* **source**: Where the action originated from.
* **additionalData**: Optional JSON data providing more context about the action.

### Serialization

Audits can be converted to JSON using `AuditToJsonVisitor`:

```java
Audit audit = ...;
JsonObject json = audit.accept(new AuditToJsonVisitor().withAdditionalData());
```

Log Messages
------------

Log messages are used for system logging. They are internationalized and can be associated with a specific element via a locator.

### Log Message fields

* **id**: Unique identifier for the log message.
* **zonedDateTime**: When the message was logged.
* **realm**: The realm in which the message occurred.
* **username**: The user associated with the event.
* **locator**: A locator pointing to the relevant model element.
* **severity**: The severity level (`Info`, `Notification`, `Warning`, `Error`, `Exception`, `System`).
* **state**: The state of the message (`Active`, `Inactive`).
* **message**: The formatted, internationalized message.

### Internationalization

`LogMessage` extends `I18nMessage`, allowing it to store a message key and values that can be resolved to a translated string
using a resource bundle.

### Serialization

`LogMessage` provides built-in JSON serialization:

```java
LogMessage message = ...;
JsonObject json = message.toJson();

// Deserialization
LogMessage fromJson = LogMessage.fromJson(json);
```

Exceptions
----------

Strolch uses runtime exceptions for error handling. The base class for all exceptions in the model module is
`StrolchException`, which extends `RuntimeException`.

### Hierarchy

* `StrolchException`: The root of the Strolch exception hierarchy.
* `StrolchModelException`: Thrown when a model-specific error occurs, such as accessing a non-existent element when existence is asserted.
* `StrolchElementNotFoundException`: A specialization of `StrolchModelException` thrown when a requested element does not exist.
* `StrolchPolicyException`: Thrown when there is an issue with policy resolution or execution.
* `StrolchAccessDeniedException`: Thrown when a privilege check fails.
* `StrolchUserMessageException`: Thrown when an error should be displayed to the user with a specific message.

### Internationalization

All Strolch exceptions support internationalization through the `I18nMessage` class. This allows the framework to carry
localized error messages that can be resolved to a specific language by the UI or other consumers.

### Modifiability Checks

When an element is marked as read-only, any attempt to modify it will throw a `StrolchModelException`. Consumers should use
`assertNotReadonly()` to explicitly check this before performing operations, or use `ensureModifiable()` on root elements
to automatically receive a mutable clone if the current element is read-only.
 
Builders
--------
 
The `li.strolch.model.builder` package provides a fluent API for constructing Strolch elements programmatically. This is
particularly useful for creating test data, migrations, or default model fragments in code.
 
The builders follow a nested structure that mirrors the Strolch element hierarchy.
 
### Root Element Builders
 
Use `ResourceBuilder`, `OrderBuilder`, or `ActivityBuilder` as the entry point for creating root elements.
 
#### Resource Example
 
```java
Resource resource = new ResourceBuilder("product01", "My Product", "Product")
	.bag("parameters", "Parameters")
		.string("color", "Color").value("Red").end()
		.integer("weight", "Weight").value(10).end()
	.endBag()
	.floatState("stock", "Stock").end()
	.build();
```
 
#### Order Example
 
```java
Order order = new OrderBuilder("order01", "Order 1", "OrderType")
	.bag("parameters", "Parameters")
		.date("expectedDate", "Expected Date").value(ZonedDateTime.now()).end()
	.endBag()
	.build();
```
 
#### Activity Example
 
```java
Activity activity = new ActivityBuilder("production", "Production Process", "Process", TimeOrdering.SERIES)
	.action("setup", "Setup", "SetupTask")
		.bag("parameters", "Parameters")
			.duration("duration", "Duration").value(Duration.ofMinutes(30)).end()
		.endBag()
	.endAction()
	.subActivity("assembly", "Assembly", "AssemblyTask", TimeOrdering.PARALLEL)
		.action("weld", "Welding", "WeldTask").endAction()
		.action("bolt", "Bolting", "BoltTask").endAction()
	.endSubActivity()
	.build();
```
 
### Parameter Bag and Parameter Builders
 
Every root element builder provides methods to add parameter bags. Within a `BagBuilder`, you can add various typed parameters.
 
* `bag(id, name, type)`: Adds a new parameter bag.
* `defaultBag()`: Adds/accesses the conventional `parameters` bag.
* `relationsBag()`: Adds/accesses the conventional `relations` bag.
* `endBag()`: Returns to the parent root element builder.
 
Parameters are added within a bag:
* `string(id)`, `integer(id)`, `booleanB(id)`, etc.: Adds a typed parameter builder.
* `value(T value)`: Sets the parameter value.
* `interpretation(String)`, `uom(String)`: Sets metadata.
* `end()`: Returns to the `BagBuilder`.
 
### Relationships in Builders
 
The builders provide convenience methods for defining relationships, automatically setting the correct interpretation and UOM:
 
```java
Resource resource = new ResourceBuilder("res01", "Resource 1", "Type1")
	.resourceRelation("parent", "ParentResource") // 1-to-1 to a Resource of type "ParentResource"
	.orderRelations("tasks", "OrderType")         // 1-to-N to Orders of type "OrderType"
	.build();
```
 
### Policies in Builders
 
Root elements and actions support defining policy implementations:
 
```java
Order order = new OrderBuilder("order01", "Order 1", "OrderType")
	.policies()
		.execution("li.strolch.execution.MyExecutionPolicy")
	.endPolicies()
	.build();
```
 
### StrolchElementBuilder for Templates
 
The `StrolchElementBuilder` class acts as a factory for building multiple elements or templates. It allows defining templates
once and then creating new instances from them.
 
```java
StrolchElementBuilder factory = new StrolchElementBuilder();
 
// Define a template
factory.resourceTemplate("Product Template", "Product")
	.defaultBag()
		.string("color", "Color").value("White").end()
	.endBag();
 
// Create a new instance from the template
Resource p1 = factory.newResource("Product", "Product 1");
Resource p2 = factory.newResource("Product", "Product 2");
p2.setString("color", "Blue");
```
 
XML contract
------------

The authoritative XML contract is `src/main/resources/StrolchModel.xsd`. It defines:

* `StrolchModel` as the document root;
* `IncludeFile` entries for loading additional model fragments;
* `Resource`, `Order`, and `Activity` root element structures;
* `ParameterBag` and `Parameter` structures;
* `Policies` and `Policy` structures;
* `TimedState` and timestamped `Value` entries;
* `Action` and `ValueChange` structures;
* allowed state, time ordering, parameter, and timed state value types.

XML importers and exporters must preserve the element IDs, names, types, parameter value types, policy definitions, timed state values,
and activity hierarchy. When adding new model fields, update the Java model, XML/JSON visitors, tests, and `StrolchModel.xsd` together.

JSON serialization
------------------

The `li.strolch.model.json` package provides visitors for converting Strolch elements to and from JSON. There are two primary
styles of serialization: **Full Serialization** and **Flat Serialization**.

### Full Serialization

Full serialization is the default style. It follows the same semantic structure as the XML contract, preserving all metadata,
parameter bags, and typed parameter information. This style is used when the complete state of an element needs to be
exchanged or persisted.

Key characteristics:
* Root elements keep their identity (`id`), `name`, and `type`.
* Parameter bags are nested under a `parameterBags` object.
* Each parameter is an object containing its `id`, `name`, `type`, `index`, and `value`.
* Activities preserve their complete child hierarchy.

Java example:
```java
Resource resource = ...;
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor());
```

Example:
```json
{
  "objectType": "Resource",
  "id": "product01",
  "name": "My Product",
  "type": "Product",
  "parameterBags": {
    "parameters": {
      "id": "parameters",
      "name": "Parameters",
      "type": "Parameters",
      "parameters": {
        "color": {
          "id": "color",
          "name": "Color",
          "type": "String",
          "value": "Red"
        }
      }
    }
  }
}
```

### Flat Serialization

Flat serialization maps parameter values directly to the resulting JSON object. This style is often used for REST APIs where a
simpler, more concise representation is preferred.

Use `StrolchRootElementToJsonVisitor.flat()` to enable this style.

Key characteristics:
* Parameters are mapped as `key: value` pairs directly on the root object.
* The nested structure of parameter bags and parameter metadata is omitted.
* Multiple bags are merged; if parameter IDs overlap, later values overwrite earlier ones (conventionally, avoid ID overlaps).
* To read flat JSON back into Strolch elements, use `FromFlatJsonVisitor`.

Java examples:
```java
// Serialize to flat JSON
Resource resource = ...;
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor().flat());

// Read flat JSON back into a Strolch element
JsonObject json = ...;
Resource resource = ...;
resource.accept(new FromFlatJsonVisitor(json));
```

Example:
```json
{
  "objectType": "Resource",
  "id": "product01",
  "name": "My Product",
  "type": "Product",
  "color": "Red"
}
```

### Flat Bag Serialization

A hybrid approach allows flattening only specific parameter bags while keeping others in full format, or keeping the bags but
flattening their parameters.

* `flatBags(String... bagIds)`: Flatten the parameters within specific bags, but keep the bags as objects under the root element.
* `flatBagsByType(String... bagTypes)`: Same as above, but selecting bags by their type.

Java examples:
```java
// Flatten only the "parameters" bag
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor().flatBags("parameters"));

// Flatten all bags of type "Relations"
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor().flatBagsByType("Relations"));
```

Example of `flatBags("parameters")`:
```json
{
  "objectType": "Resource",
  "id": "product01",
  "name": "My Product",
  "type": "Product",
  "parameters": {
    "color": "Red"
  }
}
```

### Advanced Customization

The `StrolchRootElementToJsonVisitor` provides several options to customize the resulting JSON.

#### Serialization Options

You can control which metadata fields are included in the JSON output:

```java
StrolchRootElementToJsonVisitor visitor = new StrolchRootElementToJsonVisitor()
    .withoutObjectType()     // Exclude "objectType" field
    .withoutVersion()        // Exclude version information
    .withoutElementName()    // Exclude "name" field
    .withoutPolicies()       // Exclude policy definitions
    .withoutStateVariables() // Exclude timed states (Resources only)
    .withoutValueChanges()   // Exclude value changes (Actions only)
    .withLocator();          // Include the "locator" field
```

#### Ignoring Elements

Specific bags, parameters, or timed states can be excluded from serialization:

```java
StrolchRootElementToJsonVisitor visitor = new StrolchRootElementToJsonVisitor()
    .ignoreBag("internalMetadata")
    .ignoreBagByType("Internal")
    .ignoreParameter("secretToken")
    .ignoreTimeState("obsoleteState");
```

#### List Parameters as Arrays

By default, list parameters are serialized as comma-separated strings in flat mode. Use `withListParametersAsArray()` to serialize
them as JSON arrays:

```java
// Flat serialization with list parameters as arrays
JsonObject json = resource.accept(new StrolchRootElementToJsonVisitor()
    .flat()
    .withListParametersAsArray());
```

Example output:
```json
{
  "objectType": "Resource",
  "id": "product01",
  "tags": ["electronics", "premium"]
}
```

#### Activity Depth

When serializing Activities, you can control how many levels of the child hierarchy are included using `activityDepth(int)`:

```java
// Serialize only the root activity and its direct children
JsonObject json = activity.accept(new StrolchRootElementToJsonVisitor().activityDepth(1));
```

#### Hooks

Hooks allow you to add custom fields or modify the JSON object during the serialization process for different element types:

```java
StrolchRootElementToJsonVisitor visitor = new StrolchRootElementToJsonVisitor()
    .resourceHook((resource, json) -> json.addProperty("customField", "val"))
    .orderHook((order, json) -> json.addProperty("orderDate", order.getDate().toString()))
    .activityHook((activity, json) -> json.addProperty("elementCount", activity.getElements().size()))
    .actionHook((action, json) -> json.addProperty("target", action.getResourceId()))
    .bagHook((bag, json) -> {
        if (bag.getId().equals("parameters")) {
            json.addProperty("bagSource", "external-system");
        }
    });
```

Read-only and cloning rules
---------------------------

Model objects can be mutable or read-only. Shared objects returned by persistence or transaction layers may be marked read-only to
avoid accidental changes. Before mutating an element, code should operate on a modifiable instance or explicitly call
`ensureModifiable()` where appropriate.

Use `getClone()` when a deep copy is required. Cloning is expected to preserve the semantic content of an element, including parameter
bags, policies, timed states, and activity children.

Implementation guidelines
-------------------------

When changing `strolch-model`:

* keep the Java model, XML schema, JSON visitors, builders, and tests consistent;
* prefer typed parameter and timed state classes over generic string encoding;
* use visitor APIs for traversal and conversion logic;
* maintain read-only checks on mutating methods;
* keep root element identity based on object type, type, and ID;
* add or update sample XML files when the XML contract changes;
* run the relevant module tests for behavior changes.

Testing
-------

Documentation-only changes do not require test execution. Behavioral or schema changes should be validated with the `strolch-model`
test suite:

```bash
cd strolch/strolch-model
mvn test
```