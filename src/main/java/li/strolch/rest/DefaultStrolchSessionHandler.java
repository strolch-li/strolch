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

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultStrolchSessionHandler extends StrolchComponent implements StrolchSessionHandler {

	private static final String SESSION_ORIGIN = "session.origin";
	private static final String PROP_VALIDATE_ORIGIN = "validateOrigin";
	private PrivilegeHandler privilegeHandler;
	private Map<String, Certificate> certificateMap;
	private boolean validateOrigin;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultStrolchSessionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		this.validateOrigin = configuration.getBoolean(PROP_VALIDATE_ORIGIN, false);
		super.initialize(configuration);
	}

	@Override
	public void start() {
		this.privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
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

		Certificate certificate = this.privilegeHandler.authenticate(username, password);
		certificate.getSessionDataMap().put(SESSION_ORIGIN, origin);
		this.certificateMap.put(certificate.getAuthToken(), certificate);

		return certificate;
	}

	@Override
	public Certificate validate(String origin, String authToken) {
		DBC.PRE.assertNotEmpty("Origin must be set!", origin);
		DBC.PRE.assertNotEmpty("SessionId must be set!", authToken);

		Certificate certificate = this.certificateMap.get(authToken);
		if (certificate == null)
			throw new StrolchException(MessageFormat.format("No certificate exists for sessionId {0}", authToken));

		this.privilegeHandler.isCertificateValid(certificate);

		if (this.validateOrigin && !origin.equals(certificate.getSessionDataMap().get(SESSION_ORIGIN))) {
			String msg = MessageFormat.format("Illegal request for origin {0} and sessionId {1}", origin, authToken);
			throw new StrolchException(msg);
		}

		return certificate;
	}

	@Override
	public void invalidateSession(String origin, Certificate certificate) {
		DBC.PRE.assertNotEmpty("Origin must be set!", origin);
		DBC.PRE.assertNotNull("Certificate must bet given!", certificate);

		if (this.validateOrigin && !origin.equals(certificate.getSessionDataMap().get(SESSION_ORIGIN))) {
			String msg = MessageFormat.format("Illegal request for origin {0} and sessionId {1}", origin,
					certificate.getAuthToken());
			throw new StrolchException(msg);
		}

		Certificate removedCert = this.certificateMap.remove(certificate.getAuthToken());
		if (removedCert == null)
			logger.error("No session was registered with token " + certificate.getAuthToken());

		this.privilegeHandler.invalidateSession(certificate);
	}
}
