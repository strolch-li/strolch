/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent;

import org.eclipse.store.storage.embedded.types.EmbeddedStorageManager;
import org.junit.Test;

import java.nio.file.Path;
import java.time.ZonedDateTime;

import static org.eclipse.store.storage.embedded.types.EmbeddedStorage.Foundation;

public class SimpleEclipseStoreTests {

	public static final String TARGET_PATH = "target/" + SimpleEclipseStoreTests.class.getSimpleName();

	@Test
	public void testMultipleStarts() {

		for (int i = 0; i < 5; i++) {
			run();
		}
	}

	private static void run() {
		try (EmbeddedStorageManager storageManager = Foundation(Path.of(TARGET_PATH)).setDataBaseName("Test").start()) {

			// print the last loaded root instance,
			// replace it with a current version and store it
			System.out.println(storageManager.root());
			storageManager.setRoot("Hello World! @ " + ZonedDateTime.now());
			storageManager.storeRoot();

			// shutdown storage
			storageManager.shutdown();
		}
	}
}
