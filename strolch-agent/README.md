# Strolch Agent

The Strolch Agent is the core runtime for Strolch applications. It manages the lifecycle of components, handles configuration, and provides the fundamental APIs for interacting with the Strolch model.

## Documentation

Detailed documentation for various aspects of the Strolch Agent can be found in the `docs/` directory:

- [Agent Runtime and Configuration](docs/runtime.md) - Overview of the agent, its lifecycle, and how it is configured.
- [Transaction API](docs/transactions.md) - How to use transactions to interact with the data model.
- [Policy Handler](docs/policies.md) - Dependency injection and extensibility mechanism.
- [Search API](docs/search.md) - Fluent API for querying the Strolch model.
- [Service API](docs/services.md) - Encapsulating business logic in services.
- [Job Handler](docs/jobs.md) - Background tasks and scheduled jobs.
- [Operations Log](docs/operationslog.md) - Centralized operational logging.
- [Mail Handler](docs/mail.md) - Sending email notifications.
- [Audit Handler](docs/audits.md) - Recording security-relevant events and data changes.
- [Enum Handler](docs/enums.md) - Managing and querying enumerated values.
- [Realms and Multi-tenancy](docs/realms.md) - Mandate separation and data store configuration.
- [Sessions and Privileges](docs/sessions.md) - Authentication, authorization, and session management.

## Key Concepts

- **Resources, Orders, Activities**: The core elements of the Strolch data model.
- **StrolchComponent**: The base class for all handlers and services in the agent.
- **RealmHandler**: Manages mandate separation (multi-tenancy) via `StrolchRealm`s.
- **StrolchTransaction**: Ensures data consistency and handles locking and auditing.
- **Privilege**: Strolch's fine-grained security model.
