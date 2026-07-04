### Strolch Transaction API

Transactions in Strolch are the primary way to interact with the data model (Resources, Orders, Activities). They ensure data consistency, handle locking, and manage audits.

#### Opening a Transaction

Transactions are opened via the `StrolchAgent` or a `StrolchComponent`. 

```java
try (StrolchTransaction tx = agent.openTx(certificate, "MyAction", readOnly)) {
    // Perform operations
}
```

- **`certificate`**: Identifies the user and their privileges.
- **`action`**: A string naming the action, used for auditing and logging.
- **`readOnly`**: A boolean flag. 
    - If `true`, the transaction is strictly read-only. Any attempt to modify the model or add commands will throw an exception.
    - If `false`, the transaction is writeable.

#### Transaction Outcome and Best Practices

For writeable transactions where changes are made, you **must** explicitly define the outcome. If a transaction is closed with uncommitted changes, an exception will be thrown.

The recommended pattern for writeable transactions is:

```java
try (StrolchTransaction tx = agent.openTx(certificate, "UpdateResource", false).rollbackOnFailure()) {
    // ... perform operations ...
    tx.commitOnClose();
}
```

- `rollbackOnFailure()`: Configures the transaction to automatically roll back if an exception occurs. This avoids the "modified elements which will not be committed" exception that can mask the original error.
- `commitOnClose()`: Must be called at the end of the block to ensure all changes (added/updated/removed elements and commands) are persisted when the transaction closes successfully.
- `rollbackOnClose()`: Can be used to explicitly roll back all changes upon closing.

#### Interacting with the Model

While `StrolchTransaction` provides access to `ResourceMap`, `OrderMap`, and `ActivityMap`, **these should never be used directly**. Instead, use the convenience methods provided by the `StrolchTransaction` class.

##### Retrieving and Finding Elements
- `getResourceBy(type, id, assertExists)` / `getOrderBy(...)` / `getActivityBy(...)`: Retrieves a root element.
- `getResourceBy(StringParameter refP, assertExists)`: Retrieves an element referenced by a parameter.
- `findElement(locator)`: Finds any element (Resource, Order, Activity, Bag, Parameter, etc.) by its locator.
- `findParameterOnHierarchy(element, parentParamKey, bagKey, paramKey)`: Searches for a parameter up a defined hierarchy (e.g., following relations).

##### Modifying Elements
- `add(element)`: Adds a new root element.
- `update(element)`: Updates an existing root element.
- `remove(element)`: Removes a root element.
- `addOrUpdate(element)`: Adds the element if it doesn't exist, otherwise updates it.

#### Locking

Strolch uses a locking mechanism to ensure thread safety. **Elements are never locked automatically upon retrieval.** You must explicitly lock elements you intend to modify.

- `tx.lock(element)` or `tx.lock(locator)`: Acquires a lock on the specified element.
- `tx.readLock(element)`: **Recommended when modifying.** This method acquires a lock and then retrieves a *fresh copy* of the element from the database, ensuring you are working with the latest data under lock.

#### Commands

Complex or reusable business logic should be encapsulated in `Command` objects and added to the transaction. Commands are validated and executed when the transaction is flushed or committed.

The recommended pattern is to instantiate the command, configure it, and then add it to the transaction:

```java
try (StrolchTransaction tx = openTx(certificate, "MyAction", false).rollbackOnFailure()) {
    MyCommand command = new MyCommand(tx);
    command.setArg1(value1);
    command.setArg2(value2);
	
	// either
	command.validate();
	command.doCommand();
	
	// or
    command.validateAndDoCommand();

    tx.commitOnClose();
}
```

Two lifecycle methods are available for commands:
- `tx.addCommand(command)`: Adds the command to the transaction for execution on commit. In which case the following methods are called automatically:
    - `command.validate()`: Called automatically before execution to verify preconditions.
    - `command.doCommand()`: Called automatically to perform the business logic.

Or perform the command in your open TX:
- `command.validate()`: Verify preconditions.
- `command.doCommand()`: Perform the business logic.
or
- `command.validateAndDoCommand()`: To preconditions validation and perform the business logic in one step.

In the event of failure:
- `command.undo()`: Called automatically if the transaction fails and needs to roll back, or can be called manually.

#### Auditing and Privileges

- **Auditing**: All write operations are automatically audited if enabled. Auditing can be suppressed for specific transactions using `tx.suppressAudits()`.
- **Privilege Assertions**: Use the transaction to verify user permissions:
    - `tx.assertHasPrivilege(Operation.UPDATE, resource)`: Throws `AccessDeniedException` if the user lacks the privilege.

#### AutoCloseable

Transactions implement `AutoCloseable` and **must** be used with try-with-resources. This ensures that the transaction state is finalized, resources are freed, and all acquired locks are released, regardless of whether the operations succeeded or failed.
