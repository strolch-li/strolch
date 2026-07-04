li.strolch.model
================

`strolch-model` contains the core object model used by Strolch agents, persistence modules, searches, services, and REST APIs.
It provides the Java representation of Strolch root elements, parameter containers, policies, timed states, activities, audit records,
log messages, XML/JSON conversion, and builder APIs.

The module is intentionally independent from persistence and runtime agent concerns. It defines what a Strolch model is and how it
can be represented, cloned, traversed, serialized, and modified in memory.

Core concepts
-------------

### Root elements

Strolch models are built from three root element types:

* `Resource` represents a managed entity, such as a machine, product, stock item, location, or person.
* `Order` represents a task, request, or transaction that can move through a state lifecycle.
* `Activity` represents executable or plannable work. Activities can contain nested `Activity` elements and `Action` elements.

All root elements have an `Id`, `Name`, and `Type`. They also support parameter bags and policy definitions. `Order` and `Action`
can carry a `State`, while `Activity` defines a `TimeOrdering` (`Series` or `Parallel`) for its child elements.

### Parameter bags and parameters

Parameters are grouped in `ParameterBag` instances. A parameter bag is identified by an ID and contains typed `Parameter` values.
This enables models to be extended without changing Java classes or database schemas.

Supported parameter value types are defined by `StrolchModel.xsd` and include:

* `Boolean`
* `String`
* `Text`
* `Integer`
* `Long`
* `Float`
* `Date`
* `Duration`
* `StringList`
* `IntegerList`
* `FloatList`
* `LongList`

Use the convenience methods on `ParameterBagContainer` whenever possible instead of manually retrieving a bag and then a parameter:

```java
String articleNumber = resource.getString("articleNumber");
resource.setInteger("stock", 12);
FloatParameter weightP = resource.getFloatP("weight");
```

For non-default bags, pass the bag ID explicitly:

```java
String externalId = resource.getString("integration", "externalId");
```

### Timed states

`Resource` elements can contain `StrolchTimedState` instances. A timed state stores values over time and is used for schedules,
state histories, stock levels, counters, or measured values. Timed states can be filtered by `Interpretation` and `Uom`.

Supported timed state value types are:

* `Boolean`
* `Integer`
* `Float`
* `Long`
* `FloatList`
* `StringSet`

### Policies

Policies attach configurable behavior to root elements or activity elements. A model element stores `PolicyDef` entries by policy
type. Runtime modules can resolve these definitions to concrete policy implementations.

### Relationships

Relationships between root elements are defined using parameters. By convention, these are stored in a `ParameterBag` with the ID
`relations`. Relationships can be 1-to-1 (using `StringParameter`) or 1-to-N (using `StringListParameter`). Convenience methods like
`setRelation()` and `getRelationId()` simplify managing these references.

### Activities and actions

Activities model work structures. They can contain:

* child `Activity` elements, which allow hierarchical plans;
* `Action` elements, which can reference a target resource by `ResourceId` and `ResourceType`;
* `ValueChange` entries, which describe changes to timed states at specific times.

`Activity` and `Action` implement hierarchical parameter lookup, so a child element can find parameters defined on a parent activity.

### Locators and visitors

Model elements expose `Locator` values that identify their position in the model hierarchy. Visitors are available for traversing
and converting model elements, including XML and JSON conversion visitors.

### Audits and log messages

The model includes specialized elements for system events:

* `Audit` records track access and changes to elements, capturing "who, what, and when".
* `LogMessage` provides internationalized system logging, which can be associated with specific model elements via locators.

### Exceptions

Strolch uses runtime exceptions for error handling. The base class is `StrolchException`, and most model-related errors
throw `StrolchModelException`. All exceptions support internationalization via `I18nMessage`.

### Cloning and read-only handling

Model elements support deep cloning through `getClone()`. Elements can also be marked read-only with `setReadOnly()` or checked with
`ensureModifiable()` and `ensureReadOnly()`. Persistence and transaction layers use these mechanisms to protect shared model state.

XML model format
----------------

The XML representation is defined by `src/main/resources/StrolchModel.xsd`. A `StrolchModel` document can contain `Resource`,
`Order`, and `Activity` root elements as well as `IncludeFile` entries for splitting a model into multiple files.

Example:

```xml
<StrolchModel xmlns="https://strolch.li/schema/StrolchModel.xsd">
    <Resource Id="product-1" Name="Product 1" Type="Product">
        <ParameterBag Id="parameters" Name="Parameters" Type="Parameters">
            <Parameter Id="articleNumber" Name="Article Number" Type="String" Value="A-1000"/>
            <Parameter Id="stock" Name="Stock" Type="Integer" Value="12"/>
        </ParameterBag>
    </Resource>

    <Order Id="order-1" Name="Order 1" Type="Order" State="Created">
        <ParameterBag Id="parameters" Name="Parameters" Type="Parameters">
            <Parameter Id="priority" Name="Priority" Type="String" Value="normal"/>
        </ParameterBag>
    </Order>
</StrolchModel>
```

Builder API
-----------

The `li.strolch.model.builder` package contains fluent builders for root elements, parameter bags, parameters, policies, activities,
and actions. Builders ensure that element construction is concise, consistent, and follows the hierarchical requirements of the
model.

JSON serialization
------------------

Strolch supports two primary styles of JSON serialization:

* **Full serialization**: Preserves the complete metadata and typed structure of parameters, mirroring the XML format.
* **Flat serialization**: Maps parameters directly as `key: value` pairs on the root object, ideal for concise REST APIs.

Serialization behavior can be further customized by ignoring specific elements, using hooks, or controlling the depth of
activity hierarchies.

Related documentation
---------------------

See `docs/strolch-model.md` for the technical module specification.