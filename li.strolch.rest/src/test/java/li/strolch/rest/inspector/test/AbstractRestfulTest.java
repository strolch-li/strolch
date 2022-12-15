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
package li.strolch.rest.inspector.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Base64;
import java.util.Collections;
import java.util.logging.Level;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.rest.StrolchRestfulClasses;
import li.strolch.rest.endpoint.Inspector;
import li.strolch.rest.filters.AuthenticationRequestFilter;
import li.strolch.testbase.runtime.RuntimeMock;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.grizzly2.servlet.GrizzlyWebContainerFactory;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;
import org.glassfish.jersey.server.TracingConfig;
import org.glassfish.jersey.servlet.ServletProperties;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.spi.TestContainer;
import org.glassfish.jersey.test.spi.TestContainerException;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public abstract class AbstractRestfulTest extends JerseyTest {

	public static final String AUTHENTICATION_PATH = "strolch/authentication";

	protected static final Logger logger = LoggerFactory.getLogger(AbstractRestfulTest.class);
	private static final String RUNTIME_PATH = "target/withPrivilegeRuntime/";
	private static final String CONFIG_SRC = "src/test/resources/withPrivilegeRuntime";
	private static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() throws IllegalArgumentException, IOException {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	protected String authenticate() {
		return authenticate("jill", "jill");
	}

	protected void logout(String authToken) {
		logout("jill", authToken);
	}

	protected String authenticate(String username, String password) {

		// login
		JsonObject login = new JsonObject();
		login.addProperty("username", username);
		login.addProperty("password", Base64.getEncoder().encodeToString(username.getBytes()));
		Entity<String> entity = Entity.entity(login.toString(), MediaType.APPLICATION_JSON);

		Response result = target() //
				.path(AUTHENTICATION_PATH) //
				.request(MediaType.APPLICATION_JSON) //
				.post(entity);
		assertEquals(Response.Status.OK.getStatusCode(), result.getStatus());

		JsonObject loginResult = JsonParser.parseString(result.readEntity(String.class)).getAsJsonObject();
		assertEquals("jill", loginResult.get("username").getAsString());
		assertEquals(64, loginResult.get("authToken").getAsString().length());
		assertNull(loginResult.get("msg"));

		return loginResult.get("authToken").getAsString();
	}

	protected void logout(String username, String authToken) {

		Response result = target() //
				.path(AUTHENTICATION_PATH + "/" + authToken) //
				.request(MediaType.APPLICATION_JSON) //
				.delete();
		assertEquals(Response.Status.OK.getStatusCode(), result.getStatus());

		JsonObject logoutResult = JsonParser.parseString(result.readEntity(String.class)).getAsJsonObject();
		assertEquals("jill has been logged out.", logoutResult.get("msg").getAsString());
	}

	@Override
	protected ResourceConfig configure() {
		forceEnable(TestProperties.LOG_TRAFFIC);
		enable(TestProperties.DUMP_ENTITY);
		return createApp();
	}

	public static ResourceConfig createApp() {
		ResourceConfig resourceConfig = new ResourceConfig();
		resourceConfig.setApplicationName("RestTest");
		resourceConfig.registerClasses(StrolchRestfulClasses.restfulClasses);
		resourceConfig.registerClasses(StrolchRestfulClasses.providerClasses);

		LoggingFeature loggingFeature = new LoggingFeature(
				java.util.logging.Logger.getLogger(LoggingFeature.DEFAULT_LOGGER_NAME), Level.SEVERE,
				LoggingFeature.Verbosity.PAYLOAD_ANY, null);
		resourceConfig.register(loggingFeature);
		resourceConfig.property(ServerProperties.TRACING, TracingConfig.ALL.name());
		resourceConfig.property(ServletProperties.FILTER_FORWARD_ON_404, true);
		return resourceConfig;
	}

	@Override
	protected TestContainerFactory getTestContainerFactory() throws TestContainerException {

		return new TestContainerFactory() {
			@Override
			public TestContainer create(URI baseUri, DeploymentContext deploymentContext) {
				return new TestContainer() {
					private HttpServer server;

					@Override
					public ClientConfig getClientConfig() {
						return null;
					}

					@Override
					public URI getBaseUri() {
						return baseUri;
					}

					@Override
					public void start() {
						try {
							this.server = GrizzlyWebContainerFactory.create(baseUri,
									Collections.singletonMap("jersey.config.server.provider.packages",
											Inspector.class.getPackage().getName() + ";"
													+ AuthenticationRequestFilter.class.getPackage().getName()));
						} catch (ProcessingException | IOException e) {
							throw new TestContainerException(e);
						}
					}

					@Override
					public void stop() {
						this.server.shutdownNow();
					}
				};

			}
		};
	}
}
