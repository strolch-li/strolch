### Strolch Service API

Services are the primary way to implement business logic in Strolch. They provide a structured way to execute operations, handle transactions, and enforce security.

#### Concepts

-   **`Service<T, U>`**: Interface for a service that takes an argument of type `T` and returns a result of type `U`.
-   **`ServiceArgument`**: Base class for all service arguments.
-   **`ServiceResult`**: Base class for all service results, containing state (SUCCESS, FAILURE), messages, and optionally the output data.
-   **`ServiceHandler`**: Component responsible for executing services and managing their lifecycle and security context.

#### Implementing a Service

To implement a service, extend `AbstractService<T, U>` and implement the `internalDoService` method. It is recommended to encapsulate the actual business logic in one or more `Command` objects.

```java
public class MyService extends AbstractService<MyArgument, MyResult> {
    @Override
    protected MyResult internalDoService(MyArgument arg) throws Exception {
        Object result;
        try (StrolchTransaction tx = openArgOrUserTx(arg).rollbackOnFailure()) {
            
			// Business logic encapsulated in a command
            command = new MyCommand(tx);
            command.setSomeData(arg.someData);
            command.validateAndDoCommand();
            
            result = command.getResult();
            
            tx.commitOnClose();
        }
        
        // Return result from command
        return new MyResult(result);
    }

    @Override
    protected MyResult getResultInstance() {
        return new MyResult();
    }

    @Override
    public MyArgument getArgumentInstance() {
        return new MyArgument();
    }
}
```

**Note:** Even though you can implement your own `ServiceArgument` and `ServiceResult` classes, it is recommended that you use one of the predefined classes in the `strolch-service` module. These classes already implement the most common arguments and results and make it easier to reason on. 

#### Executing a Service

Services are executed via the `ServiceHandler`.

```java
ServiceHandler serviceHandler = agent.getComponent(ServiceHandler.class);
MyService service = new MyService();
StringArgument arg = new StringArgument("someValue");
ServiceResult result = serviceHandler.doService(certificate, service, arg);

if (result.isOk()) {
    // Service succeeded
} else {
    // Handle error
}
```

#### Key Features

-   **Transaction Management**: `AbstractService` provides convenience methods to open transactions (`openArgOrUserTx`, `openUserTx`).
-   **Security**: Services are checked against the user's privileges before execution.
-   **Auditing**: Service executions can be automatically audited by the `AuditHandler`.
-   **Error Handling**: Exceptions thrown within `internalDoService` are caught and wrapped in a `ServiceResult` with a failure state.

#### Service Arguments and Results

-   Use **Java Records** for simple DTOs used as arguments or results.
-   `ServiceResult` provides static factory methods like `success()` and `error(String)`.
