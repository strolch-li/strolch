/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ch.eitchnet.communication;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
public class AbstractEndpointTest {

	static final Logger logger = LoggerFactory.getLogger(FileEndpointTest.class);

	public static TestIoMessage createTestMessage(String key1, String key2, String connectionId) {
		return createTestMessage(CommandKey.key(key1, key2), connectionId);
	}

	@SuppressWarnings("nls")
	public static TestIoMessage createTestMessage(CommandKey key, String connectionId) {
		TestIoMessage msg = new TestIoMessage(UUID.randomUUID().toString(), key, connectionId);
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
