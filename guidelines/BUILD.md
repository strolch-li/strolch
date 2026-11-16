# Build and Configuration

The project is a multi-module Maven project.

## Prerequisites

- Java (JDK 25 or higher recommended, as it is common for modern Strolch projects). Code should be written using latest
  Java 24 concepts, APIs, etc.
- Maven 3.6+.

## Build Instructions

To build the entire project from the root:

```bash
mvn clean install
```

If you want to skip tests (e.g., for faster initial setup):

```bash
mvn clean install -DskipTests
```

## Publishing to Maven Central

The `deploy-maven-central` profile uses `central-publishing-maven-plugin` 0.12.0. This version fixes bundle validation
failures caused by Maven 3.10.0 staging metadata, such as `maven-metadata-local.xml` and `_remote.repositories`. Use
Maven 3.9 or newer when publishing to Maven Central.

Start a release retry with a clean build so stale staging files from a previous attempt are not bundled:

```bash
mvn clean deploy -Pdeploy-maven-central
```

## Key Modules

TBD