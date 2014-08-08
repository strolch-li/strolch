package ch.eitchnet.communication;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AbstractEndpointTest {

	static final Logger logger = LoggerFactory.getLogger(FileEndpointTest.class);

	public static TestIoMessage createTestMessage(String key1, String key2) {
		return createTestMessage(CommandKey.key(key1, key2));
	}

	@SuppressWarnings("nls")
	public static TestIoMessage createTestMessage(CommandKey key) {
		TestIoMessage msg = new TestIoMessage(UUID.randomUUID().toString(), key);
		List<String> lines = new ArrayList<>();
		lines.add("bla");
		lines.add("foo");
		lines.add("bar");
		lines.add("bla");
		msg.setContents(lines);
		return msg;
	}

	protected void waitForMessage(TestConnectionObserver observer) throws InterruptedException {
		long start = System.currentTimeMillis();
		while (observer.getMessage() == null) {
			if (System.currentTimeMillis() - start > 2000)
				fail("Connection didn't send message in 2s!"); //$NON-NLS-1$
			Thread.sleep(50);
		}
	}
}
