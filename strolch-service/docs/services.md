# Strolch Services

Services are the primary entry points for business logic in Strolch. They are designed to be called from REST resources, UIs, or other high-level components.

## Core Concepts

### AbstractService

All services should inherit from `li.strolch.service.api.AbstractService<T, U>`, where `T` is the argument type and `U` is the result type.

Key responsibilities of `AbstractService`:
- Transaction management: It facilitates opening and closing transactions.
- Privilege checking: It ensures the user has the necessary permissions.
- Result handling: It ensures a consistent result object is returned.

#### Example

```java
public class MyService extends AbstractService<StringArgument, ServiceResult> {
    @Override
    protected ServiceResult internalDoService(StringArgument arg) throws Exception {
        try (StrolchTransaction tx = openArgOrUserTx(arg)) {
            // Perform business logic
            tx.commitOnClose();
        }
        return ServiceResult.success();
    }
}
```

### ServiceResult

Services return a `ServiceResult` (or a subclass). It indicates whether the operation was successful and carries any result data or error messages.

- `ServiceResult.success()`: Returns a successful result.
- `ServiceResult.error(String msg)`: Returns an error result with a message.

### Arguments

Services take an argument extending `ServiceArgument`. Common arguments include:
- `StringArgument`
- `StrolchRootElementArgument`
- `LocatorArgument`

## Commands

Commands are atomic, reusable operations that are performed within a transaction. They are typically called from within a `Service`.

### Command

All commands should inherit from `li.strolch.service.api.Command`.

Key methods to implement:
- `validate()`: Perform pre-condition checks.
- `doCommand()`: The actual logic.
- `undo()`: Optional. Logic to revert changes if the transaction is rolled back (Strolch's transaction management often handles this automatically for model changes).

#### Example

```java
public class MyCommand extends Command {
    private Resource resource;

    public MyCommand(StrolchTransaction tx) {
        super(tx);
    }

    public void setResource(Resource resource) {
        this.resource = resource;
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

## Generic CRUD Services

The `strolch-service` module provides a set of generic services for standard operations:

- `AddResourceService`, `UpdateResourceService`, `RemoveResourceService`
- `AddOrderService`, `UpdateOrderService`, `RemoveOrderService`
- `AddActivityService`, `UpdateActivityService`, `RemoveActivityService`
- `AddOrUpdateStrolchRootElementService`: Handles adding or updating any root element automatically.

## Best Practices

- **Transactions**: Always use try-with-resources for `StrolchTransaction`.
- **Privileges**: Use `tx.assertHasPrivilege()` or similar methods to enforce security.
- **Commands**: Delegate complex or reusable logic to `Command` objects.
- **Validation**: Perform input validation before starting heavy logic or transactions.
