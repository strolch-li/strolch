/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.runtime.configuration;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.utils.helper.FileHelper;
import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertEquals;

public class BootstrapperTest {

	@Test
	public void shouldSetupByUserDir() {

		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/shouldSetupByUserDir/runtime");
		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete target " + rootDstPath);
		}
		if (!rootDstPath.mkdirs()) {
			throw new RuntimeException("Could not create target " + rootDstPath);
		}

		if (!FileHelper.copy(rootSrcPath.listFiles(), rootDstPath, false)) {
			throw new RuntimeException("Could not copy source " + rootSrcPath + " to " + rootDstPath);
		}

		String userDir = System.getProperty("user.dir");
		System.setProperty("user.dir", rootDstPath.getParentFile().getAbsolutePath());
		try {
			StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByUserDir("dev", "runtime");
			assertEquals("dev", agent.getStrolchConfiguration().getRuntimeConfiguration().getEnvironment());
		} finally {
			System.setProperty("user.dir", userDir);
		}
	}

	@Test
	public void shouldSetupByRoot() {

		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/shouldSetupByRoot");
		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete target " + rootDstPath);
		}
		if (!rootDstPath.mkdirs()) {
			throw new RuntimeException("Could not create target " + rootDstPath);
		}

		if (!FileHelper.copy(rootSrcPath.listFiles(), rootDstPath, false)) {
			throw new RuntimeException("Could not copy source " + rootSrcPath + " to " + rootDstPath);
		}

		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByRoot("dev", rootDstPath);
		assertEquals("dev", agent.getStrolchConfiguration().getRuntimeConfiguration().getEnvironment());
	}

	@Test
	public void shouldSetupByCopyingRoot() {

		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/shouldSetupByCopyingRoot");
		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete target " + rootDstPath);
		}

		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByCopyingRoot("dev", rootSrcPath,
				rootDstPath);
		assertEquals("dev", agent.getStrolchConfiguration().getRuntimeConfiguration().getEnvironment());
	}

	@Test
	public void shouldSetupByBoostrapFileEnvDev() {

		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/bootstraptest/dev");
		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete target " + rootDstPath);
		}
		if (!rootDstPath.mkdirs()) {
			throw new RuntimeException("Could not create target " + rootDstPath);
		}

		if (!FileHelper.copy(rootSrcPath.listFiles(), rootDstPath, false)) {
			throw new RuntimeException("Could not copy source " + rootSrcPath + " to " + rootDstPath);
		}

		File bootstrapFile = new File("src/test/resources/bootstraptest/StrolchBootstrap.xml");
		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByBootstrapFile("dev",
				bootstrapFile);
		assertEquals("dev", agent.getStrolchConfiguration().getRuntimeConfiguration().getEnvironment());
		agent.destroy();
	}

	@Test
	public void shouldSetupByBoostrapFileEnvTest() {

		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/bootstraptest/test");
		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete target " + rootDstPath);
		}
		if (!rootDstPath.mkdirs()) {
			throw new RuntimeException("Could not create target " + rootDstPath);
		}

		File configSrc = new File(rootSrcPath, "config");
		File configDst = new File(rootDstPath, "config_other");
		if (!configDst.mkdir()) {
			throw new RuntimeException("Could not create target " + configDst);
		}

		if (!FileHelper.copy(configSrc.listFiles(), configDst, false)) {
			throw new RuntimeException("Could not copy source " + configSrc + " to " + configDst);
		}

		File bootstrapFile = new File("src/test/resources/bootstraptest/StrolchBootstrap.xml");

		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByBootstrapFile("test.next",
				bootstrapFile);
		assertEquals("test", agent.getStrolchConfiguration().getRuntimeConfiguration().getEnvironment());
		agent.destroy();
	}
}
