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

import static org.junit.Assert.assertEquals;

import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleMessageArchiveTest extends AbstractEndpointTest {

	@Test
	public void testArchive() throws InterruptedException {

		IoMessageArchive archive = new SimpleMessageArchive(20, 5);

		CommandKey key = CommandKey.key("key1", "key2"); //$NON-NLS-1$//$NON-NLS-2$
		String connectionId = "connection1"; //$NON-NLS-1$

		int i = 0;
		for (; i < 20; i++) {
			TestIoMessage msg = new TestIoMessage(UUID.randomUUID().toString(), key, connectionId);
			// update the time by plus 1, otherwise the tree set does not add it
			msg.setUpdated(new Date(i + 1));
			archive.archive(msg);
		}

		assertEquals(20, archive.size());

		// add one more
		TestIoMessage msg = new TestIoMessage(UUID.randomUUID().toString(), key, connectionId);
		msg.setUpdated(new Date(i + 1));
		archive.archive(msg);

		// validate the trimming works
		assertEquals(15, archive.size());

		// Now make sure our last element is still in the list
		List<IoMessage> all = archive.getAll();
		Collections.sort(all, new Comparator<IoMessage>() {
			@Override
			public int compare(IoMessage o1, IoMessage o2) {
				return o1.getUpdated().compareTo(o2.getUpdated());
			}
		});
		IoMessage message = all.get(all.size() - 1);
		assertEquals(msg.getId(), message.getId());
	}
}
