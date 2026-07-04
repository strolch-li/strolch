# strolch-utils

The `strolch-utils` module is a collection of project-independent Java utility classes and helpers designed to simplify common programming tasks. It covers areas such as Design by Contract (DBC), object modification tracking, date/time handling, specialized collections, and various helper utilities for common Java types.

## Dependencies

This utility package is built by Maven and has very few external dependencies. The current dependencies include:

- Java 25 (Recommended)
- SLF4J (Logging API)
- GSON (JSON processing)
- Jakarta XML Binding (JAXB)
- Jakarta Mail
- Bouncy Castle (Cryptography)

## Features

- **Design by Contract (DBC)**: Assertions for preconditions, postconditions, and intermediate checks.
- **ObjectFilter**: Efficient tracking of object modifications (Add/Update/Remove).
- **ISO8601**: Standardized date and time handling.
- **Specialized Collections**: `MapOfLists`, `MapOfSets`, `MapOfMaps`, and `Paging` utilities.
- **Helper Classes**: Comprehensive helpers for Strings, Files, XML, Dates, Exceptions, and more.
- **I18n Utilities**: Framework for internationalized messages.
- **Database Utilities**: Helpers for database connection and schema management.

For more technical details, see [docs/strolch-utils.md](docs/strolch-utils.md).

## Building

### Prerequisites

- JDK 25 is installed.
- Maven 3.6+ is installed.

### Build Instructions

To build the project:

```bash
mvn clean install
```
