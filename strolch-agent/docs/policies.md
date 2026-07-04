### Strolch Policy Handler

The `PolicyHandler` is Strolch's mechanism for dependency injection and extensibility. It allows defining interchangeable logic that can be selected at runtime based on the data model.

#### Concepts

-   **Policy Definition (`PolicyDef`)**: A reference to a policy type and a key.
-   **Policy Implementation (`StrolchPolicy`)**: The actual Java class that implements the logic.
-   **Policy Configuration**: Policies are configured in an XML file (e.g., `StrolchPolicies.xml`) or within the element's XML definition.

#### Defining a Policy

A policy is defined by its type (e.g., `ExecutionPolicy`, `PlanningPolicy`) and a key (e.g., `DefaultExecution`).

```xml
<Policies>
    <Policy type="ExecutionPolicy" value="key:DefaultExecution"/>
</Policies>
```

#### Implementing a Policy

To implement a policy, extend `StrolchPolicy` and implement the required interface.

```java
public class MyExecutionPolicy extends StrolchPolicy implements ExecutionPolicy {
    public MyExecutionPolicy(StrolchTransaction tx) {
        super(tx);
    }

    @Override
    public void execute(IActivityElement element) {
        // Implementation logic
    }
}
```

#### Using a Policy

Policies are retrieved within a transaction.

```java
try (StrolchTransaction tx = agent.openTx(certificate, "ExecuteActivity", false)) {
    Action action = ...;
    ExecutionPolicy policy = tx.getPolicy(action, ExecutionPolicy.class);
    policy.execute(action);
    tx.commitOnClose();
}
```

#### Retrieving a Policy without an Element

Sometimes you need a policy that is not directly attached to a model element. In this case, you can create a `PolicyDef` manually and use the `tx.getPolicy(Class<T> clazz, PolicyDef policyDef)` method.

```java
try (StrolchTransaction tx = agent.openTx(certificate, "PerformTask", false)) {
    PolicyDef policyDef = PolicyDef.getKeyPolicy(ExecutionPolicy.class, "DefaultExecution");
    ExecutionPolicy policy = tx.getPolicy(ExecutionPolicy.class, policyDef);
    policy.execute(null); // Or some other context
    tx.commitOnClose();
}
```

Alternatively, you can use `PolicyDef.valueOf()` to create a `PolicyDef` from its XML string representation:

```java
PolicyDef policyDef = PolicyDef.valueOf(ExecutionPolicy.class, "key:DefaultExecution");
```

Or reference a Java class directly:

```java
PolicyDef policyDef = PolicyDef.getJavaPolicy(ExecutionPolicy.class, MyExecutionPolicy.class);
```

#### Benefits

-   **Decoupling**: Business logic is decoupled from the core framework and the data model.
-   **Flexibility**: Different implementations can be used for different types of elements or even specific element instances.
-   **Testability**: Policies can be easily mocked or replaced in tests.
