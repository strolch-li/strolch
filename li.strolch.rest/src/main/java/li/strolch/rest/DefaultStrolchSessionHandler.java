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
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultStrolchSessionHandler extends StrolchComponent implements StrolchSessionHandler {

	private static final Logger logger = LoggerFactory.getLogger(DefaultStrolchSessionHandler.class);
	private static final String PARAM_SESSION_TTL_MINUTES = "session.ttl.minutes"; //$NON-NLS-1$
	private PrivilegeHandler privilegeHandler;
	private Map<String, Certificate> certificateMap;
	private long sessionTtl;
	private Timer sessionTimeoutTimer;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultStrolchSessionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		this.sessionTtl = TimeUnit.MINUTES.toMillis(configuration.getInt(PARAM_SESSION_TTL_MINUTES, 30));
		super.initialize(configuration);
	}

	@Override
	public void start() {
		this.privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
		this.certificateMap = new HashMap<>();

		this.sessionTimeoutTimer = new Timer("SessionTimeoutTimer");
		long checkInterval = TimeUnit.MINUTES.toMillis(1);
		this.sessionTimeoutTimer.schedule(new SessionTimeoutTask(), checkInterval, checkInterval);

		super.start();
	}

	@Override
	public void stop() {
		if (this.certificateMap != null) {
			synchronized (this.certificateMap) {
				for (Certificate certificate : this.certificateMap.values()) {
					this.privilegeHandler.invalidateSession(certificate);
				}
				this.certificateMap.clear();
			}
		}

		if (this.sessionTimeoutTimer != null) {
			this.sessionTimeoutTimer.cancel();
		}

		this.sessionTimeoutTimer = null;
		this.privilegeHandler = null;
		super.stop();
	}

	@Override
	public void destroy() {
		this.certificateMap = null;
		super.destroy();
	}

	@Override
	public Certificate authenticate(String username, byte[] password) {
		DBC.PRE.assertNotEmpty("Origin must be set!", username); //$NON-NLS-1$
		DBC.PRE.assertNotEmpty("Username must be set!", username); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Passwort must be set", password); //$NON-NLS-1$

		synchronized (this.certificateMap) {
			Certificate certificate = this.privilegeHandler.authenticate(username, password);
			certificate.setLastAccess(System.currentTimeMillis());
			this.certificateMap.put(certificate.getAuthToken(), certificate);

			logger.info(this.certificateMap.size() + " sessions currently active.");
			return certificate;
		}
	}

	@Override
	public Certificate validate(String authToken) {
		DBC.PRE.assertNotEmpty("SessionId must be set!", authToken); //$NON-NLS-1$

		Certificate certificate;
		synchronized (this.certificateMap) {
			certificate = this.certificateMap.get(authToken);
		}

		if (certificate == null)
			throw new StrolchException(MessageFormat.format("No certificate exists for sessionId {0}", authToken)); //$NON-NLS-1$

		return validate(certificate);
	}

	@Override
	public Certificate validate(Certificate certificate) {
		this.privilegeHandler.isCertificateValid(certificate);
		certificate.setLastAccess(System.currentTimeMillis());
		return certificate;
	}

	@Override
	public void invalidateSession(Certificate certificate) {
		DBC.PRE.assertNotNull("Certificate must bet given!", certificate); //$NON-NLS-1$

		Certificate removedCert;
		synchronized (this.certificateMap) {
			removedCert = this.certificateMap.remove(certificate.getAuthToken());
		}
		if (removedCert == null)
			logger.error(MessageFormat.format("No session was registered with token {0}", certificate.getAuthToken())); //$NON-NLS-1$

		this.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @return the certificateMap
	 */
	protected Map<String, Certificate> getCertificateMap() {
		return this.certificateMap;
	}

	/**
	 * Simpler {@link TimerTask} to check for sessions which haven't been active for
	 * {@link DefaultStrolchSessionHandler#PARAM_SESSION_TTL_MINUTES} minutes.
	 * 
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	private class SessionTimeoutTask extends TimerTask {

		@Override
		public void run() {

			Map<String, Certificate> map = getCertificateMap();
			Map<String, Certificate> certificateMap;
			synchronized (map) {
				certificateMap = new HashMap<>(map);
			}

			long reqLastAccessTime = System.currentTimeMillis() - sessionTtl;

			for (Certificate certificate : certificateMap.values()) {
				if (certificate.getLastAccess() < reqLastAccessTime) {
					String msg = "Session {0} for user {1} has expired, invalidating session...";
					logger.info(MessageFormat.format(msg, certificate.getAuthToken(), certificate.getUsername()));
					invalidateSession(certificate);
				}
			}
		}
	}
}
