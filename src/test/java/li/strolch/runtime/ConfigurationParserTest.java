package li.strolch.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;

import org.junit.Test;

@SuppressWarnings("nls")
public class ConfigurationParserTest {

	@Test
	public void shouldParseConfigurationFile() {

		File rootPathF = new File("src/test/resources/configtest");
		StrolchConfiguration strolchConfiguration = ConfigurationParser.parseConfiguration(rootPathF);
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
		assertEquals("li.strolch.service.ServiceHandler", serviceHandlerConfiguration.getApi());
		assertEquals("li.strolch.service.SimpleServiceHandler", serviceHandlerConfiguration.getImpl());
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
		assertEquals("PrivilegeConfig.xml", privilegeHandlerConfiguration.getString("privilegeConfigFile", null));

		//	<Component>
		//		<name>PersistenceHandler</name>
		//		<api>li.strolch.persistence.api.StrolchPersistenceHandler</api>
		//		<impl>li.strolch.persistence.impl.XmlPersistenceHandler</impl>
		//		<Properties>
		//			<verbose>true</verbose>
		//		</Properties>
		//	</Component>
		ComponentConfiguration persistenceHandlerConfiguration = strolchConfiguration
				.getComponentConfiguration("PersistenceHandler");
		assertNotNull("Should have created a PersistenceHandler Configuration", persistenceHandlerConfiguration);
		assertEquals("PersistenceHandler", persistenceHandlerConfiguration.getName());
		assertEquals("li.strolch.persistence.api.StrolchPersistenceHandler", persistenceHandlerConfiguration.getApi());
		assertEquals("li.strolch.persistence.impl.XmlPersistenceHandler", persistenceHandlerConfiguration.getImpl());
		assertEquals(1, persistenceHandlerConfiguration.getPropertyKeys().size());
		assertEquals(true, persistenceHandlerConfiguration.getBoolean("verbose", null));
	}
}
