### Strolch Agent Runtime

The Strolch Agent is the core runtime of the Strolch framework. It manages the lifecycle of components, handles configuration, and provides access to the Strolch Container.

#### StrolchAgent

The `StrolchAgent` class is the central entry point. It provides methods to:
- Access components via the `ComponentContainer`.
- Open transactions (`StrolchTransaction`).
- Run code with specific privileges (`runAs`, `runAsAgent`).
- Access executor services for asynchronous tasks.
- Retrieve system state and version information.
- Access `AgentStatistics` for performance metrics.

#### AgentStatistics

The `AgentStatistics` class tracks various performance metrics of the Strolch Agent. These statistics are useful for monitoring and identifying performance bottlenecks.

Tracked metrics include:
- **Transactions**: Number and duration of successful transactions.
- **Failed Transactions**: Number and duration of failed transactions.
- **Searches**: Number and duration of search operations.
- **Services**: Number and duration of service executions.
- **Logons**: Number of logons and current active users.
- **Retried Locks**: Number of times a lock was retried.

To access the statistics:

```java
AgentStatistics stats = agent.getAgentStatistics();
JsonObject json = stats.toJson();
```

The statistics are also available via the REST API if the management components are enabled.

#### Configuration

Strolch configuration is typically defined in an XML file (e.g., `StrolchConfiguration.xml`). It consists of two main parts:

1.  **Runtime Configuration**: Defines global settings like application name, environment, locale, timezone, and paths (config, data, temp).
2.  **Component Configuration**: Defines specific settings for each `StrolchComponent`.

##### Runtime Configuration Example

```xml
<Runtime>
    <applicationName>My Strolch App</applicationName>
    <environment>dev</environment>
    <locale>en_US</locale>
    <timezone>Europe/Zurich</timezone>
</Runtime>
```

##### Component Configuration Example

```xml
<Component>
    <name>MyComponent</name>
    <api>com.example.MyComponent</api>
    <impl>com.example.MyComponentImpl</impl>
    <Properties>
        <someProperty>someValue</someProperty>
    </Properties>
</Component>
```

#### Component Container

The `ComponentContainer` manages the lifecycle of all `StrolchComponent` instances. Components are started in the order they are defined (respecting dependencies) and stopped in reverse order.

#### StrolchComponent

All handlers and services in Strolch are implemented as `StrolchComponent`s. A component has the following lifecycle methods:

- `setup(ComponentConfiguration configuration)`: Called during agent setup.
- `initialize(ComponentConfiguration configuration)`: Called during agent initialization.
- `start()`: Called when the agent starts.
- `stop()`: Called when the agent stops.
- `destroy()`: Called when the agent is destroyed.

Components can access other components via the container and open transactions to interact with the data model.
