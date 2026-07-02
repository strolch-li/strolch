/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.job;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class ClearTempPathJobTest {

	private File targetPath;

	@Before
	public void setUp() {
		targetPath = new File("target/" + ClearTempPathJobTest.class.getSimpleName());
		if (targetPath.exists())
			deleteRecursive(targetPath);
		targetPath.mkdirs();
	}

	@After
	public void tearDown() {
		deleteRecursive(targetPath);
	}

	private void deleteRecursive(File file) {
		File[] files = file.listFiles();
		if (files != null) {
			for (File f : files) {
				deleteRecursive(f);
			}
		}
		file.delete();
	}

	@Test
	public void shouldGetTempRetention() {
		Map<String, String> values = new HashMap<>();
		values.put(RuntimeConfiguration.PROP_TEMP_RETENTION_DEFAULT, "P10D");
		values.put(RuntimeConfiguration.PROP_TEMP_RETENTION_PREFIX + "test", "P1D");

		RuntimeConfiguration config = new RuntimeConfiguration("test", "test", values,
				new File("src/test/resources/configtest/config"), new File("target"), targetPath, Set.of());

		assertEquals(Duration.ofDays(10), config.getTempRetention("default"));
		assertEquals(Duration.ofDays(1), config.getTempRetention("test"));
		assertEquals(Duration.ofDays(10), config.getTempRetention("unknown"));
		assertEquals(0, config.getTempRetentionKeep("default"));
	}

	@Test
	public void shouldGetTempRetentionKeep() {
		Map<String, String> values = new HashMap<>();
		values.put(RuntimeConfiguration.PROP_TEMP_RETENTION_KEEP_DEFAULT, "5");
		values.put(RuntimeConfiguration.PROP_TEMP_RETENTION_KEEP_PREFIX + "test", "10");

		RuntimeConfiguration config = new RuntimeConfiguration("test", "test", values,
				new File("src/test/resources/configtest/config"), new File("target"), targetPath, Set.of());

		assertEquals(5, config.getTempRetentionKeep("default"));
		assertEquals(10, config.getTempRetentionKeep("test"));
		assertEquals(5, config.getTempRetentionKeep("unknown"));
	}

	@Test
	public void shouldClearOldFiles() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/minimaltest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();
			RuntimeConfiguration runtimeConfig = agent.getRuntimeConfiguration();

			File tempPath = runtimeConfig.getTempPath();

			// Prepare files
			File prefixDir = new File(tempPath, "test_prefix");
			if (!prefixDir.exists() && !prefixDir.mkdirs())
				throw new IOException("Failed to create prefix dir " + prefixDir.getAbsolutePath());

			File oldFile = new File(prefixDir, "old.txt");
			if (!oldFile.createNewFile())
				throw new IOException("Failed to create old file " + oldFile.getAbsolutePath());
			oldFile.setLastModified(System.currentTimeMillis() - Duration.ofDays(8).toMillis());

			File newFile = new File(prefixDir, "new.txt");
			if (!newFile.createNewFile())
				throw new IOException("Failed to create new file " + newFile.getAbsolutePath());

			// Enable deletion
			Map<String, String> properties1 = agent.getRuntimeConfiguration().getAsMap();
			properties1.put(RuntimeConfiguration.PROP_TEMP_RETENTION_DELETE_ENABLED, "true");
			agent.getRuntimeConfiguration().updateProperties(properties1);

			// Run job
			ClearTempPathJob job = new ClearTempPathJob(agent, "test", "test", JobMode.Manual);
			job.execute(null);

			// Verify (default retention is P7D)
			assertFalse("Old file should be deleted", oldFile.exists());
			assertTrue("New file should still exist", newFile.exists());
		}
	}

	@Test
	public void shouldKeepAtLeastNFiles() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/minimaltest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();

			// Mock keep property
			Map<String, String> properties = agent.getRuntimeConfiguration().getAsMap();
			properties.put(RuntimeConfiguration.PROP_TEMP_RETENTION_KEEP_PREFIX + "keep_test", "2");
			agent.getRuntimeConfiguration().updateProperties(properties);

			File tempPath = agent.getRuntimeConfiguration().getTempPath();

			// Prepare files
			File prefixDir = new File(tempPath, "keep_test");
			if (!prefixDir.exists() && !prefixDir.mkdirs())
				throw new IOException("Failed to create prefix dir " + prefixDir.getAbsolutePath());

			// Create 3 old files
			File old1 = new File(prefixDir, "old1.txt");
			old1.createNewFile();
			old1.setLastModified(System.currentTimeMillis() - Duration.ofDays(10).toMillis());

			File old2 = new File(prefixDir, "old2.txt");
			old2.createNewFile();
			old2.setLastModified(System.currentTimeMillis() - Duration.ofDays(9).toMillis());

			File old3 = new File(prefixDir, "old3.txt");
			old3.createNewFile();
			old3.setLastModified(System.currentTimeMillis() - Duration.ofDays(8).toMillis());

			// Enable deletion
			properties.put(RuntimeConfiguration.PROP_TEMP_RETENTION_DELETE_ENABLED, "true");
			agent.getRuntimeConfiguration().updateProperties(properties);

			// Run job
			ClearTempPathJob job = new ClearTempPathJob(agent, "test", "test", JobMode.Manual);
			job.execute(null);

			// Verify: old2 and old3 should be kept because keep=2 (and they are newer than old1)
			assertFalse("old1 should be deleted (oldest)", old1.exists());
			assertTrue("old2 should be kept (one of the 2 newest)", old2.exists());
			assertTrue("old3 should be kept (one of the 2 newest)", old3.exists());
		}
	}

	@Test
	public void shouldKeepAtLeastNRootFiles() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/minimaltest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();

			// Mock keep property for default
			Map<String, String> properties = agent.getRuntimeConfiguration().getAsMap();
			properties.put(RuntimeConfiguration.PROP_TEMP_RETENTION_KEEP_DEFAULT, "1");
			agent.getRuntimeConfiguration().updateProperties(properties);

			File tempPath = agent.getRuntimeConfiguration().getTempPath();

			// Create 2 old files in root
			File old1 = new File(tempPath, "old1.txt");
			old1.createNewFile();
			old1.setLastModified(System.currentTimeMillis() - Duration.ofDays(10).toMillis());

			File old2 = new File(tempPath, "old2.txt");
			old2.createNewFile();
			old2.setLastModified(System.currentTimeMillis() - Duration.ofDays(9).toMillis());

			// Enable deletion
			properties.put(RuntimeConfiguration.PROP_TEMP_RETENTION_DELETE_ENABLED, "true");
			agent.getRuntimeConfiguration().updateProperties(properties);

			// Run job
			ClearTempPathJob job = new ClearTempPathJob(agent, "test", "test", JobMode.Manual);
			job.execute(null);

			// Verify: old2 should be kept because keep=1
			assertFalse("old1 should be deleted (oldest)", old1.exists());
			assertTrue("old2 should be kept (newest)", old2.exists());
		}
	}

	@Test
	public void shouldSimulateDeletion() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/minimaltest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();

			// Ensure deletion is disabled (default)
			Map<String, String> properties = agent.getRuntimeConfiguration().getAsMap();
			properties.put(RuntimeConfiguration.PROP_TEMP_RETENTION_DELETE_ENABLED, "false");
			agent.getRuntimeConfiguration().updateProperties(properties);

			File tempPath = agent.getRuntimeConfiguration().getTempPath();

			// Create old file
			File oldFile = new File(tempPath, "old.txt");
			oldFile.createNewFile();
			oldFile.setLastModified(System.currentTimeMillis() - Duration.ofDays(100).toMillis());

			// Run job
			ClearTempPathJob job = new ClearTempPathJob(agent, "test", "test", JobMode.Manual);
			job.execute(null);

			// Verify: old file should still exist because we are in simulation mode
			assertTrue("old file should still exist in simulation mode", oldFile.exists());
		}
	}
}
