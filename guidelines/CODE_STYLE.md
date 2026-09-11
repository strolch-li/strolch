# Code Style

- **Language**: All code, documentation, comments, and commit messages must be in **English**. Even if requirements are
  provided in another language (e.g., German), the implementation and its documentation must be in English.
- **Localisations**: When adding localisations in German, always use Swiss German (e.g., never use the `ß` character).
- **Indentation**: Use **Tabs** for indentation, not spaces.
- **Code Layout**:
    - Consistent indentation is crucial for readability. Using tabs instead of spaces ensures that the code looks the
      same on all systems.
    - Keep lines of code under 160 characters.
    - When wrapping lines, break after a comma or an operator. Indent the new line with two tabs.
- **File Size**: Classes and files should ideally not exceed **1000 lines of code**. This is not a hard limit; use
  common sense and break up classes if it makes sense logically and improves maintainability.
- **License Header**: All source files should include the `atexxi Systems AG` copyright header. However, for Strolch
  code, which is open source, its original license must be respected.
- **Naming Conventions**:
    - Package names should be all lowercase, without underscores or other special characters. They should be short,
      meaningful, and based on the project's domain.
    - Classes/Interfaces: `PascalCase` and should be nouns or noun phrases.
    - Methods: `camelCase` and should be verbs or verb phrases.
    - Variables: `camelCase` and should be short and meaningful. Avoid single-letter variable names except for loop
      counters.
    - Constants: `UPPER_SNAKE_CASE` (e.g., `TYPE_PERSON`, `BAG_PARAMETERS`). Centrally defined in `ModelConstants.java`
      classes within each module where applicable.

### Best Practices for Classes, Interfaces, and Enums

#### Use Records for data holder classes

Prefer using Java records for storing data holder classes.

```java
// Good
public record CustomerDTO(String name, String email) {
}
```

#### Immutability

Prefer immutable classes whenever possible. Immutable objects are inherently thread-safe and make the code easier to
reason about.

#### Use Interfaces

Program to interfaces, not implementations. This makes the code more flexible and easier to test.

```java
// Good
List<String> names = new ArrayList<>();
```

#### Use Enums

Use enums instead of string constants or integer constants. Enums are type-safe and provide more readable and
maintainable code.

### Exception Handling

- **Catch Specific Exceptions**: Catch specific exceptions instead of `Exception` or `Throwable`.
- **Don't Ignore Exceptions**: Never ignore exceptions. If you catch an exception, either handle it or rethrow it.

### Concurrency

- **Use `java.util.concurrent`**: Prefer the high-level concurrency utilities in the `java.util.concurrent` package over
  low-level primitives like `wait()` and `notify()`.
- **Avoid `volatile` for Complex Operations**: Use `volatile` only for simple atomic operations. For more complex
  operations, use `java.util.concurrent.atomic` or locks.

### Use of `Optional`

- **Return Types**: Use `Optional` for return types when a method might not return a value. This makes the API clearer
  and helps prevent `NullPointerException`.
- **Don't Use for Fields or Parameters**: Do not use `Optional` for class fields or method parameters. For optional
  dependencies, use method overloading or a nullable annotation.

### Stream API Best Practices

- **Avoid Side Effects**: Avoid side effects in stream operations like `map()` and `filter()`.
- **Prefer Method References**: Prefer method references over lambdas when possible.

### Collections

- **Use the Right Collection**: Choose the right collection for the job. Use `List` for ordered collections, `Set` for
  unordered collections with no duplicates, and `Map` for key-value pairs.
- **Prefer `isEmpty()` over `size() == 0`**.
- **Return Empty Collections, Not Null**: Methods that return collections should return an empty collection instead of
  `null`.
- **Use Diamond Operator**: Use the diamond operator (`<>`) for generic type inference.
- **Use `for-each` loop**: Prefer the `for-each` loop for iterating over collections.

### Date and Time

Prefer using the Java 8 Date-Time API (`java.time.*`) over legacy `java.util.Date` and `java.util.Calendar`. The
`java.time` API is immutable, thread-safe, and more expressive.

### Strings

Use text blocks (`"""`), available since Java 15, for multi-line string literals (e.g., SQL, JSON, XML) instead of
concatenation or `\n` escapes.

### Debugging

- **Logging**: Use SLF4J with `LoggerFactory.getLogger(Class.class)`. When possible always use SLF4J, never `System.out`
  or `printStackTrace` and related methods.
- **Strolch Transactions**: Ensure transactions are properly committed or rolled back.

### Documentation

When a new feature is implemented, the `README.md` file in the root of the repository (not in atx-dev directly) must be
updated (or created if it doesn't exist) to document the feature for the end user. This documentation should include
instructions on how to use and/or configure the new feature.

Furthermore, a technical specification of the feature must also be documented in a `docs/` directory at the root of the
repository (or within the module if specific to it).
