# Strolch Utils

The `strolch-utils` module is a collection of project-independent Java utility classes and helpers designed to simplify common programming tasks. It covers areas such as Design by Contract (DBC), object modification tracking, date/time handling, specialized collections, and various helper utilities for common Java types.

## Key Components

### Design by Contract (DBC)
The `DBC` (Design by Contract) enum provides a set of assertion methods to enforce preconditions, postconditions, and intermediate checks within the code. Using DBC improves code reliability by catching invalid states early.

- `DBC.PRE`: Used for preconditions (e.g., validating method arguments).
- `DBC.INTERIM`: Used for intermediate checks during processing.
- `DBC.POST`: Used for postconditions (e.g., validating results before returning).

#### Example
```java
public void setAge(int age) {
    DBC.PRE.assertTrue("Age must be positive", age > 0);
    this.age = age;
}
```

### ObjectFilter
The `ObjectFilter` is a utility for tracking modifications (add, update, remove) to a set of objects. It is particularly useful when performing batch operations where multiple changes might occur to the same object before the final state needs to be persisted.

It ensures that only the necessary operations are performed at the end:
- Adding then removing an object results in no operation.
- Adding then updating an object results in an "add" operation with the latest state.
- Updating an object multiple times results in a single "update" operation.

### ISO8601 Date/Time
Strolch uses ISO 8601 for date and time representation. The `li.strolch.utils.iso8601` package provides helpers for parsing and formatting dates in a consistent manner, ensuring interoperability between different systems.

### Specialized Collections
The `li.strolch.utils.collections` package contains several specialized collection classes that extend the standard Java Collections Framework:
- `MapOfLists`: A map where each key maps to a list of values.
- `MapOfSets`: A map where each key maps to a set of values.
- `MapOfMaps`: A nested map structure.
- `Paging`: A utility for handling paginated data sets.

### Helper Classes
A wide range of helper classes are available in `li.strolch.utils.helper` and `li.strolch.utils` to simplify operations on common Java types and tasks:
- `StringHelper`: String manipulation, validation, and conversion.
- `FileHelper`: File and directory operations (copy, delete recursively, read/write).
- `XmlHelper`: XML parsing, transformation, and formatting.
- `DateHelper`: Date manipulation and calculations.
- `ExceptionHelper`: Utilities for handling and formatting exceptions.
- `SystemHelper`: Access to system-specific information and OS-specific tasks.
- `ClassScanningHelper`: Utilities for filtering files and classes during classpath scanning (e.g., ignoring common libraries and metadata).

### I18n Utilities
The `I18nUtils` and `I18nMessage` classes provide a framework for handling internationalized messages, allowing for easy localization of user-facing strings.

### Database Utilities
The `li.strolch.db` package provides utilities for database management, including data source building, schema version checks, and migration state tracking.

### Time Utilities
The `li.strolch.utils.time` package provides advanced time manipulation utilities, including:
- `PeriodHelper`: Shifting dates by periods, calculating days in periods, and handling multiple period shifts.
- `Interval`: Represents a time interval with a start and end point.
- `PeriodDuration`: A wrapper for ISO 8601 durations.

### Other Utilities
- **GS1 Barcodes**: `SimpleGs1` provides basic parsing for GS1-128 barcodes.
- **LDAP**: `LdapHelper` simplifies LDAP authentication and attribute retrieval.
- **Cryptography**: `AesCryptoHelper` provides simple AES encryption/decryption.
- **Email**: `SmtpMailer` and `SimulatedSmtpMailer` for sending emails.

## Usage

To use `strolch-utils` in a Maven project, add the following dependency:

```xml
<dependency>
    <groupId>li.strolch</groupId>
    <artifactId>strolch-utils</artifactId>
    <version>${strolch.version}</version>
</dependency>
```
