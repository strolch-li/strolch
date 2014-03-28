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
package li.strolch.rest;

import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.StrolchPrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultStrolchSessionHandler extends StrolchComponent implements StrolchSessionHandler {

	private static final String PROP_REMEMBER_USER = "rememberUser";
	private StrolchPrivilegeHandler privilegeHandler;
	private Map<String, Certificate> certificateMap;
	private boolean rememberUser;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultStrolchSessionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		this.rememberUser = configuration.getBoolean(PROP_REMEMBER_USER, false);
		super.initialize(configuration);
	}

	@Override
	public void start() {
		this.privilegeHandler = getContainer().getComponent(StrolchPrivilegeHandler.class);
		this.certificateMap = new HashMap<>();
		super.start();
	}

	@Override
	public void stop() {
		if (this.certificateMap != null) {
			for (Certificate certificate : this.certificateMap.values()) {
				this.privilegeHandler.invalidateSession(certificate);
			}
			this.certificateMap.clear();
		}
		this.privilegeHandler = null;
		super.stop();
	}

	@Override
	public void destroy() {
		this.certificateMap = null;
		super.destroy();
	}

	@Override
	public Certificate authenticate(String origin, String username, byte[] password) {
		DBC.PRE.assertNotEmpty("Origin must be set!", username);
		DBC.PRE.assertNotEmpty("Username must be set!", username);
		DBC.PRE.assertNotNull("Passwort must be set", password);

		String userId = getUserId(origin, username);
		Certificate certificate;
		if (this.rememberUser) {
			certificate = this.certificateMap.get(userId);
			if (certificate != null) {
				this.privilegeHandler.isCertificateValid(certificate);
				logger.info("Re-using session for user " + userId + " and sessionId " + certificate.getSessionId());
				return certificate;
			}
		}

		certificate = this.privilegeHandler.authenticate(username, password);
		if (this.rememberUser)
			this.certificateMap.put(userId, certificate);
		else
			this.certificateMap.put(certificate.getAuthToken(), certificate);

		return certificate;
	}

	@Override
	public Certificate validate(String origin, String username, String sessionId) {
		DBC.PRE.assertNotEmpty("Origin must be set!", username);
		DBC.PRE.assertNotEmpty("Username must be set!", username);

		Certificate certificate;
		if (this.rememberUser)
			certificate = this.certificateMap.get(getUserId(origin, username));
		else
			certificate = this.certificateMap.get(sessionId);

		if (certificate == null)
			throw new StrolchException("No certificate exists for sessionId " + sessionId);

		if (!certificate.getUsername().equals(username) || !certificate.getAuthToken().equals(sessionId)) {
			throw new StrolchException("Illegal request for username " + username + " and sessionId " + sessionId);
		}

		this.privilegeHandler.isCertificateValid(certificate);
		return certificate;
	}

	@Override
	public void invalidateSession(String origin, Certificate certificate) {
		if (this.rememberUser)
			this.certificateMap.remove(getUserId(origin, certificate.getUsername()));
		else
			this.certificateMap.remove(certificate.getSessionId());
		this.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @param origin
	 * @param username
	 * @return
	 */
	private String getUserId(String origin, String username) {
		return origin + "_" + username;
	}
}
