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
int version = element.getInteger("version");
String externalId = element.getString("integration", "externalId");
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

* `getTimedState(String id)`;
* `hasTimedState(String id)`;
* `streamOfTimedStates()`;
* `streamOfTimedStatesByInterpretation(String interpretation)`;
* `streamOfTimedStatesByInterpretationAndUom(String interpretation, String uom)`.

Policies
--------

Policy definitions are stored as a mapping from policy type to implementation value. The model only stores the definitions. Runtime
modules are responsible for resolving a `PolicyDef` to an executable policy implementation.

Policy definitions should be used when behavior must remain configurable in model data rather than hard-coded in services or commands.

Activities
----------

Activities represent structured work. An activity has a `TimeOrdering` value:

* `Series` means children are ordered sequentially.
* `Parallel` means children may be considered in parallel.

Activities can contain other activities and actions. Actions may reference target resources through `ResourceId` and `ResourceType`.
Actions can also contain value changes that describe timed state changes.

Hierarchical parameter lookup allows activity elements to reuse context from parent activities. Use `findParameter(paramId)` where a
child element should inherit a value from its activity hierarchy.

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

JSON conversion
---------------

The `li.strolch.model.json` package provides visitors for JSON conversion. JSON conversion should follow the same semantic structure
as XML conversion: root elements keep their identity and type, parameter bags keep typed parameter values, and activities keep their
child hierarchy.

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