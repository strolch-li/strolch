package ch.eitchnet.xmlpers.test;

@SuppressWarnings("nls")
public class RealmTest extends AbstractPersistenceTest {

	protected static final String BASE_PATH = "target/db/RealmTest";

	public static void beforeClass() {
		cleanPath(BASE_PATH);
	}
}
