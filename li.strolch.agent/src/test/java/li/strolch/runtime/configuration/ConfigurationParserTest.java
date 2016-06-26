/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.utils.helper.FileHelper;

@SuppressWarnings("nls")
public class ConfigurationParserTest {

	public static StrolchConfiguration parseConfiguration(Class<?> clazz, String env) {
		File rootSrcPath = new File("src/test/resources/configtest");
		File rootDstPath = new File("target/" + clazz.getSimpleName());

		if (rootDstPath.exists() && !FileHelper.deleteFile(rootDstPath, true)) {
			throw new RuntimeException("Could not delete existing target " + rootDstPath);
		}

		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByCopyingRoot(env, rootSrcPath,
				rootDstPath);
		return agent.getStrolchConfiguration();
	}

	@Test
	public void shouldParseConfigurationFileForDevEnv() {

		StrolchConfiguration strolchConfiguration = parseConfiguration(ConfigurationParserTest.class, "dev");
		assertNotNull("Should have created a configuration object", strolchConfiguration);

		//	<Runtime>
		//		<applicationName>StrolchRuntimeTest</applicationName>
		//		<Properties>
		//			<verbose>true</verbose>
		//		</Properties>
		//	</Runtime>
		RuntimeConfiguration runtimeConfiguration = strolchConfiguration.getRuntimeConfiguration();
		assertNotNull("Should have created a runtime configuration", runtimeConfiguration);
		assertEquals("StrolchRuntimeTest", runtimeConfiguration.getApplicationName());
		assertEquals("dev", runtimeConfiguration.getEnvironment());
		assertEquals(true, runtimeConfiguration.getBoolean("verbose", null));

		//	<Component>
		//		<name>ServiceHandler</name>
		//		<api>li.strolch.service.ServiceHandler</api>
		//		<impl>li.strolch.service.SimpleServiceHandler</impl>
		//		<Properties>
		//		</Properties>
		//	</Component>
		ComponentConfiguration serviceHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("ServiceHandler");
		assertNotNull("Should have created a ServiceHandler Configuration", serviceHandlerConfiguration);
		assertEquals("ServiceHandler", serviceHandlerConfiguration.getName());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTest", serviceHandlerConfiguration.getApi());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTestImpl",
				serviceHandlerConfiguration.getImpl());
		assertEquals(0, serviceHandlerConfiguration.getPropertyKeys().size());

		//	<Component>
		//		<name>PrivilegeHandler</name>
		//		<api>li.strolch.runtime.privilege.DefaultStrolchPrivilegeHandler</api>
		//		<impl>li.strolch.runtime.privilege.DefaultStrolchPrivilegeHandler</impl>
		//		<depends>PersistenceHandler</depends>
		//		<Properties>
		//			<privilegeConfigFile>PrivilegeConfig.xml</privilegeConfigFile>
		//		</Properties>
		//	</Component>
		ComponentConfiguration privilegeHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("PrivilegeHandler");
		assertNotNull("Should have created a PrivilegeHandler Configuration", privilegeHandlerConfiguration);
		assertEquals("PrivilegeHandler", privilegeHandlerConfiguration.getName());
		assertEquals("li.strolch.runtime.privilege.DefaultStrolchPrivilegeHandler",
				privilegeHandlerConfiguration.getApi());
		assertEquals("li.strolch.runtime.privilege.DefaultStrolchPrivilegeHandler",
				privilegeHandlerConfiguration.getImpl());
		assertEquals(1, privilegeHandlerConfiguration.getPropertyKeys().size());
		assertEquals(1, privilegeHandlerConfiguration.getDependencies().size());
		assertTrue(privilegeHandlerConfiguration.getDependencies().contains("PersistenceHandler"));
		assertEquals("PrivilegeConfig.xml", privilegeHandlerConfiguration.getString("privilegeConfigFile", null));

		//	<Component>
		//		<name>ResourceGeneratorHandler</name>
		//		<api>li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTest</api>
		//		<impl>li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTestImpl</impl>
		//		<Properties>
		//			<verbose>true</verbose>
		//		</Properties>
		//	</Component>
		ComponentConfiguration persistenceHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("ResourceGeneratorHandler");
		assertNotNull("Should have created a ResourceGeneratorHandler Configuration", persistenceHandlerConfiguration);
		assertEquals("ResourceGeneratorHandler", persistenceHandlerConfiguration.getName());
		assertEquals("li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTest",
				persistenceHandlerConfiguration.getApi());
		assertEquals("li.strolch.runtime.configuration.model.ResourceGeneratorHandlerTestImpl",
				persistenceHandlerConfiguration.getImpl());
		assertEquals(1, persistenceHandlerConfiguration.getPropertyKeys().size());
		assertEquals(true, persistenceHandlerConfiguration.getBoolean("verbose", null));

		//	<env id="global">
		//		<Component>
		//			<name>RealmHandler</name>
		// 			<api>li.strolch.agent.api.RealmHandler</api>
		// 			<impl>li.strolch.agent.impl.DefaultRealmHandler</impl>
		//		</Component>
		//	<env id="dev">
		//		<Component>
		//			<name>RealmHandler</name>
		//			<Properties>
		//				<foo>bar</foo>
		//			</Properties>
		//		</Component>
		ComponentConfiguration realmHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("RealmHandler");
		assertNotNull("Should have created a RealmHandler Configuration", realmHandlerConfiguration);
		assertEquals("RealmHandler", realmHandlerConfiguration.getName());
		assertEquals("li.strolch.agent.api.RealmHandler", realmHandlerConfiguration.getApi());
		assertEquals("li.strolch.agent.impl.DefaultRealmHandler", realmHandlerConfiguration.getImpl());
		assertEquals(2, realmHandlerConfiguration.getPropertyKeys().size());
		assertEquals("EMPTY", realmHandlerConfiguration.getString("dataStoreMode", null));
		assertEquals("bar", realmHandlerConfiguration.getString("foo", null));

		//		<Component>
		//			<name>AdditionalServiceHandler</name>
		//			<api>li.strolch.service.api.ServiceHandler</api>
		//			<impl>li.strolch.service.SimpleServiceHandler</impl>
		//			<Properties>
		//				<foo>bar</foo>
		//			</Properties>
		//		</Component>
		//	</env>
		ComponentConfiguration additionalServiceHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("AdditionalServiceHandler");
		assertNotNull("Should have created a AdditionalServiceHandler Configuration",
				additionalServiceHandlerConfiguration);
		assertEquals("AdditionalServiceHandler", additionalServiceHandlerConfiguration.getName());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTest",
				additionalServiceHandlerConfiguration.getApi());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTestImpl",
				additionalServiceHandlerConfiguration.getImpl());
		assertEquals(1, additionalServiceHandlerConfiguration.getPropertyKeys().size());
		assertEquals("bar", additionalServiceHandlerConfiguration.getString("foo", null));
	}

	@Test
	public void shouldParseConfigurationFileForTestEnv() {

		StrolchConfiguration strolchConfiguration = parseConfiguration(ConfigurationParserTest.class, "test");
		assertNotNull("Should have created a configuration object", strolchConfiguration);

		RuntimeConfiguration runtimeConfiguration = strolchConfiguration.getRuntimeConfiguration();
		assertNotNull("Should have created a runtime configuration", runtimeConfiguration);
		assertEquals("StrolchRuntimeTest", runtimeConfiguration.getApplicationName());
		assertEquals("test", runtimeConfiguration.getEnvironment());
		assertEquals(true, runtimeConfiguration.getBoolean("verbose", null));

		//	<env id="global">
		//		<Component>
		//			<name>RealmHandler</name>
		// 			<api>li.strolch.agent.api.RealmHandler</api>
		// 			<impl>li.strolch.agent.impl.DefaultRealmHandler</impl>
		//		</Component>
		//	<env id="test">
		//		<Component>
		//			<name>RealmHandler</name>
		//			<Properties>
		//				<foo>noob</foo>
		//			</Properties>
		//		</Component>
		ComponentConfiguration realmHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("RealmHandler");
		assertNotNull("Should have created a RealmHandler Configuration", realmHandlerConfiguration);
		assertEquals("RealmHandler", realmHandlerConfiguration.getName());
		assertEquals("li.strolch.agent.api.RealmHandler", realmHandlerConfiguration.getApi());
		assertEquals("li.strolch.agent.impl.DefaultRealmHandler", realmHandlerConfiguration.getImpl());
		assertEquals(2, realmHandlerConfiguration.getPropertyKeys().size());
		assertEquals("EMPTY", realmHandlerConfiguration.getString("dataStoreMode", null));
		assertEquals("noob", realmHandlerConfiguration.getString("foo", null));

		// <Component>
		//     <name>ServiceHandler</name>
		//     <api>li.strolch.service.api.ServiceHandler</api>
		//     <impl>li.strolch.service.YetAnotherServiceHandler</impl>
		//     <depends>RealmHandler</depends>
		//     <Properties>
		//         <bar>foo</bar>
		//     </Properties>
		// </Component>
		ComponentConfiguration serviceHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("ServiceHandler");
		assertNotNull("Should have created a ServiceHandler Configuration", serviceHandlerConfiguration);
		assertEquals("ServiceHandler", serviceHandlerConfiguration.getName());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTest", serviceHandlerConfiguration.getApi());
		assertEquals("li.strolch.runtime.configuration.model.ServiceHandlerTestImpl2",
				serviceHandlerConfiguration.getImpl());
		assertEquals(2, serviceHandlerConfiguration.getDependencies().size());
		assertTrue(serviceHandlerConfiguration.getDependencies().contains("RealmHandler"));
		assertTrue(serviceHandlerConfiguration.getDependencies().contains("PrivilegeHandler"));
		assertEquals(1, serviceHandlerConfiguration.getPropertyKeys().size());
		assertEquals("foo", serviceHandlerConfiguration.getString("bar", null));
	}
}
