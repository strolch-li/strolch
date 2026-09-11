# Testing

The project uses **JUnit 4** for unit and integration testing.

### Running Tests

To run all tests in the project:

```bash
mvn test
```

To run tests in a specific module:

```bash
cd strolch-utils
mvn test
```

To run a single test class:

```bash
mvn test -Dtest=SimpleTest
```

### Adding New Tests

1. Create a new test class in the `src/test/java` directory of the relevant module.
2. Follow the naming convention `*Test.java`.
3. Use `@Test` annotation for test methods.
4. Ensure the package name matches the directory structure.

### Strolch Testing

For Strolch-specific testing (Services, Commands, Searches), please refer to the [Strolch Testing Guidelines](STROLCH_TESTING.md).

#### Example Test

```java
package ch.atexxi.model;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class SimpleTest {
	@Test
	public void testTrue() {
		assertTrue(true);
	}
}
```
