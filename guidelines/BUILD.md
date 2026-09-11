# Build and Configuration

The project is a multi-module Maven project.

### Prerequisites

- Java (JDK 25 or higher recommended, as it is common for modern Strolch projects). Code should be written using latest
  Java 24 concepts, APIs, etc.
- Maven 3.6+.

### Build Instructions

To build the entire project from the root:

```bash
mvn clean install
```

If you want to skip tests (e.g., for faster initial setup):

```bash
mvn clean install -DskipTests
```

### Key Modules

TBD