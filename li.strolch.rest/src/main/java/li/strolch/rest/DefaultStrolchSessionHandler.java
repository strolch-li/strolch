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

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_GET_SESSION;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_INVALIDATE_SESSION;

import java.text.MessageFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.privilege.model.Usage;
import li.strolch.rest.model.UserSession;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultStrolchSessionHandler extends StrolchComponent implements StrolchSessionHandler {

	public static final String PARAM_SESSION_TTL_MINUTES = "session.ttl.minutes"; //$NON-NLS-1$
	public static final String PARAM_SESSION_RELOAD_SESSIONS = "session.reload"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(DefaultStrolchSessionHandler.class);
	private PrivilegeHandler privilegeHandler;
	private Map<String, Certificate> certificateMap;
	private boolean reloadSessions;
	private long sessionTtl;
	private Timer sessionTimeoutTimer;

	public DefaultStrolchSessionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.sessionTtl = TimeUnit.MINUTES.toMillis(configuration.getInt(PARAM_SESSION_TTL_MINUTES, 30));
		this.reloadSessions = configuration.getBoolean(PARAM_SESSION_RELOAD_SESSIONS, false);
		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {
		this.privilegeHandler = getContainer().getComponent(PrivilegeHandler.class);
		this.certificateMap = Collections.synchronizedMap(new HashMap<>());

		if (this.reloadSessions) {
			List<Certificate> certificates = runAsAgentWithResult(ctx -> {
				Certificate cert = ctx.getCertificate();
				return this.privilegeHandler.getPrivilegeHandler().getCertificates(cert).stream()
						.filter(c -> !c.getUserState().isSystem()).collect(Collectors.toList());
			});
			for (Certificate certificate : certificates) {
				this.certificateMap.put(certificate.getAuthToken(), certificate);
			}

			checkSessionsForTimeout();
			logger.info("Restored " + certificates.size() + " sessions of which "
					+ (certificates.size() - this.certificateMap.size()) + " had timed out and were removed.");
		}

		this.sessionTimeoutTimer = new Timer("SessionTimeoutTimer", true); //$NON-NLS-1$
		long delay = TimeUnit.MINUTES.toMillis(5);
		long checkInterval = TimeUnit.MINUTES.toMillis(1);
		this.sessionTimeoutTimer.schedule(new SessionTimeoutTask(), delay, checkInterval);

		super.start();
	}

	@Override
	public void stop() throws Exception {
		if (this.reloadSessions) {

			runAsAgent(ctx -> getContainer().getPrivilegeHandler().getPrivilegeHandler()
					.persistSessions(ctx.getCertificate()));

		} else if (this.certificateMap != null) {
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
	public void destroy() throws Exception {
		this.certificateMap = null;
		super.destroy();
	}

	@Override
	public Certificate authenticate(String username, char[] password) {
		DBC.PRE.assertNotEmpty("Username must be set!", username); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Passwort must be set", password); //$NON-NLS-1$

		Certificate certificate = this.privilegeHandler.authenticate(username, password);
		certificate.setLastAccess(new Date());

		this.certificateMap.put(certificate.getAuthToken(), certificate);
		logger.info(MessageFormat.format("{0} sessions currently active.", this.certificateMap.size())); //$NON-NLS-1$

		return certificate;
	}

	@Override
	public Certificate validate(String authToken) throws StrolchNotAuthenticatedException {
		DBC.PRE.assertNotEmpty("authToken must be set!", authToken); //$NON-NLS-1$

		Certificate certificate = this.certificateMap.get(authToken);
		if (certificate == null)
			throw new StrolchNotAuthenticatedException(
					MessageFormat.format("No certificate exists for sessionId {0}", authToken)); //$NON-NLS-1$

		return validate(certificate);
	}

	@Override
	public Certificate validate(Certificate certificate) throws StrolchNotAuthenticatedException {
		try {
			this.privilegeHandler.isCertificateValid(certificate);
			certificate.setLastAccess(new Date());
			return certificate;
		} catch (PrivilegeException e) {
			throw new StrolchNotAuthenticatedException(e.getMessage(), e);
		}
	}

	@Override
	public void invalidate(Certificate certificate) {
		DBC.PRE.assertNotNull("Certificate must be given!", certificate); //$NON-NLS-1$

		Certificate removedCert = this.certificateMap.remove(certificate.getAuthToken());
		if (removedCert == null)
			logger.error(MessageFormat.format("No session was registered with token {0}", certificate.getAuthToken())); //$NON-NLS-1$

		this.privilegeHandler.invalidateSession(certificate);
	}

	@Override
	public void initiateChallengeFor(Usage usage, String username) {
		this.privilegeHandler.getPrivilegeHandler().initiateChallengeFor(usage, username);
	}

	@Override
	public Certificate validateChallenge(String username, String challenge) throws PrivilegeException {
		DBC.PRE.assertNotEmpty("username must be set!", username); //$NON-NLS-1$
		DBC.PRE.assertNotEmpty("challenge must be set", challenge); //$NON-NLS-1$

		Certificate certificate = this.privilegeHandler.getPrivilegeHandler().validateChallenge(username, challenge);
		certificate.setLastAccess(new Date());

		this.certificateMap.put(certificate.getAuthToken(), certificate);
		logger.info(MessageFormat.format("{0} sessions currently active.", this.certificateMap.size())); //$NON-NLS-1$

		return certificate;
	}

	protected void sessionTimeout(Certificate certificate) {
		DBC.PRE.assertNotNull("Certificate must be given!", certificate); //$NON-NLS-1$

		Certificate removedCert = this.certificateMap.remove(certificate.getAuthToken());
		if (removedCert == null)
			logger.error(MessageFormat.format("No session was registered with token {0}", certificate.getAuthToken())); //$NON-NLS-1$

		this.privilegeHandler.sessionTimeout(certificate);
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
			checkSessionsForTimeout();
		}
	}

	private void checkSessionsForTimeout() {
		Map<String, Certificate> map = getCertificateMap();
		Map<String, Certificate> certificateMap;
		synchronized (map) {
			certificateMap = new HashMap<>(map);
		}

		LocalDateTime timeOutTime = LocalDateTime.now().minus(sessionTtl, ChronoUnit.MILLIS);
		ZoneId systemDefault = ZoneId.systemDefault();

		for (Certificate certificate : certificateMap.values()) {
			Instant lastAccess = certificate.getLastAccess().toInstant();
			if (timeOutTime.isAfter(LocalDateTime.ofInstant(lastAccess, systemDefault))) {
				String msg = "Session {0} for user {1} has expired, invalidating session..."; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, certificate.getSessionId(), certificate.getUsername()));
				sessionTimeout(certificate);
			}
		}
	}

	@Override
	public UserSession getSession(Certificate certificate, String sessionId) {
		PrivilegeContext ctx = this.privilegeHandler.getPrivilegeContext(certificate);
		ctx.assertHasPrivilege(PRIVILEGE_GET_SESSION);
		synchronized (this.certificateMap) {
			for (Certificate cert : certificateMap.values()) {
				if (cert.getSessionId().equals(sessionId)) {
					ctx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_SESSION, cert));
					return new UserSession(cert);
				}
			}
		}

		throw new PrivilegeException("No Session exists with the id " + sessionId);
	}

	@Override
	public List<UserSession> getSessions(Certificate certificate) {
		PrivilegeContext ctx = this.privilegeHandler.getPrivilegeContext(certificate);
		ctx.assertHasPrivilege(PRIVILEGE_GET_SESSION);
		List<UserSession> sessions = new ArrayList<>(this.certificateMap.size());
		synchronized (this.certificateMap) {
			for (Certificate cert : certificateMap.values()) {
				try {
					ctx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_SESSION, cert));
					sessions.add(new UserSession(cert));
				} catch (AccessDeniedException e) {
					// no, user may not get this session
				}
			}
		}

		return sessions;
	}

	@Override
	public void invalidateSession(Certificate certificate, String sessionId) {
		PrivilegeContext ctx = this.privilegeHandler.getPrivilegeContext(certificate);
		ctx.assertHasPrivilege(PRIVILEGE_INVALIDATE_SESSION);

		Map<String, Certificate> map;
		synchronized (this.certificateMap) {
			map = new HashMap<>(this.certificateMap);
		}
		boolean ok = false;
		for (Certificate cert : map.values()) {
			if (cert.getSessionId().equals(sessionId)) {
				ctx.validateAction(new SimpleRestrictable(PRIVILEGE_INVALIDATE_SESSION, cert));
				invalidate(cert);
				ok = true;
				break;
			}
		}

		if (!ok) {
			throw new PrivilegeException("Can not invalidate session as no session exists with the id " + sessionId);
		}
	}

	@Override
	public void setSessionLocale(Certificate certificate, String sessionId, Locale locale) {
		if (!certificate.getSessionId().equals(sessionId)) {
			String msg = "User's can only change their own session locale: {0} may not change locale of session {1}";
			throw new AccessDeniedException(MessageFormat.format(msg, certificate.getUsername(), sessionId));
		}

		synchronized (this.certificateMap) {
			for (Certificate cert : certificateMap.values()) {
				if (cert.getSessionId().equals(sessionId)) {
					cert.setLocale(locale);
				}
			}
		}
	}
}
