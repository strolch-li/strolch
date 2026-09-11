# Strolch Testing Guidelines

Testing Strolch applications requires a specific approach to handle the in-memory model, transactions, and the component-based architecture.

### RuntimeMock

The `RuntimeMock` class (from `li.strolch.testbase.runtime`) is the central tool for Strolch testing. It allows you to programmatically set up a Strolch environment.

#### Usage
- **Initialization**: Typically done in a `@BeforeClass` method.
- **Environment**: It creates a runtime directory in `target/` and copies configuration from a source directory (e.g., `src/test/resources/runtime`).
- **Lifecycle**: Ensure you call `startContainer()` in `@BeforeClass` and `destroyRuntime()` in `@AfterClass`.

```java
private static final String TARGET_PATH = "target/" + MyTest.class.getSimpleName();
private static final String SOURCE_PATH = "src/test/resources/runtime";
private static RuntimeMock runtimeMock;

@BeforeClass
public static void beforeClass() {
	runtimeMock = new RuntimeMock().mockRuntime(TARGET_PATH, SOURCE_PATH);
	runtimeMock.startContainer();
}

@AfterClass
public static void afterClass() {
	if (runtimeMock != null)
		runtimeMock.destroyRuntime();
}
```

#### Convenience Methods
`RuntimeMock` provides several helper methods to simplify testing:
- `getContainer()`: Access the Strolch `ComponentContainer`.
- `getPrivilegeHandler()`: Access the `PrivilegeHandler` for authentication.
- `loginAdmin()` / `loginTest()`: Quick authentication.
- `openUserTx(certificate, readOnly)`: Open a transaction.

---

### Service Testing

Services are best tested by extending `AbstractServiceTest`.

- **Base Class**: `li.strolch.service.test.AbstractServiceTest`.
- **Logic**: Use `getServiceHandler().doService()` to execute the service and verify the `ServiceResult`.
- **Transactions**: `AbstractServiceTest` manages the `RuntimeMock` lifecycle.

```java
public class MyServiceTest extends AbstractServiceTest {
	@Test
	public void shouldPerformService() {
		Certificate cert = runtimeMock.loginTest();
		MyService service = new MyService();
		MyArgument arg = new MyArgument();
		arg.value = "test";

		ServiceResult result = getServiceHandler().doService(cert, service, arg);
		assertTrue(result.isOk());
	}
}
```

---

### Command Testing

Commands should be tested for both success and failure (rollback) scenarios, often across different realm types.

- **Base Class**: `li.strolch.command.AbstractRealmCommandTest`.
- **Success Case**: Implement `doCommand()` to verify the command commits correctly.
- **Rollback Case**: Implement `doCommandAsFail()` to verify that changes are rolled back on failure.
- **Multiple Realms**: The base class automatically runs tests against `Transient`, `Cached`, and `EclipseStore` realms if configured.

---

### Search Testing

Search tests verify that data retrieval logic works as expected against a populated model.

- **Population**: Use a transaction in `@BeforeClass` or a `@Before` method to add test Resources/Orders to the realm.
- **Execution**: Run the Search within a read-only transaction and assert the results.

```java
@Test
public void shouldFindResources() {
	Certificate cert = runtimeMock.loginTest();
	try (StrolchTransaction tx = runtimeMock.openUserTx(cert, true)) {
		List<Resource> results = new MyResourceSearch()
				.where(param(BAG_PARAMETERS, PARAM_COLOR, isEqualTo("blue")))
				.search(tx)
				.toList();
		assertEquals(1, results.size());
	}
}
```

### Best Practices

- **Target Directory**: Always use a unique directory under `target/` for the mocked runtime to avoid interference between parallel tests.
- **Transaction Management**: Always use try-with-resources for `StrolchTransaction`.
- **Cleanup**: Ensure `RuntimeMock.destroyRuntime()` is called to release resources and delete temporary files.
- **Privilege**: Test with appropriate user certificates to verify that privilege checks are working correctly.
