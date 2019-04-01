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
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.filters.AccessControlResponseFilter;
import li.strolch.rest.filters.HttpCacheResponseFilter;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.service.api.ServiceHandler;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RestfulStrolchComponent extends StrolchComponent {

	private static final String PARAM_CORS_ENABLED = "corsEnabled"; //$NON-NLS-1$
	private static final String PARAM_CORS_ORIGIN = "corsOrigin"; //$NON-NLS-1$
	private static final String PARAM_REST_LOGGING = "restLogging"; //$NON-NLS-1$
	private static final String PARAM_REST_LOGGING_ENTITY = "restLoggingEntity"; //$NON-NLS-1$
	private static final String PARAM_HTTP_CACHE_MODE = "httpCacheMode"; //$NON-NLS-1$
	private static final String PARAM_SECURE_COOKIE = "secureCookie"; //$NON-NLS-1$
	private static final String PARAM_COOKIE_MAX_AGE = "cookieMaxAge"; //$NON-NLS-1$

	/**
	 * Allowed values:
	 * <ul>
	 * <li>{@code OFF} - tracing support is disabled.</li>
	 * <li>{@code ON_DEMAND} - tracing support is in 'stand by' mode, it is enabled on demand by existence of request
	 * HTTP header</li>
	 * <li>{@code ALL} - tracing support is enabled for every request.</li>
	 * </ul>
	 *
	 * @see org.glassfish.jersey.server.ServerProperties#TRACING
	 */
	private static final String PARAM_REST_TRACING = "restTracing"; //$NON-NLS-1$

	/**
	 * Allowed values:
	 * <ul>
	 * <li>{@code SUMMARY}</li>
	 * <li>{@code TRACE}</li>
	 * <li>{@code VERBOSE}</li>
	 * </ul>
	 *
	 * @see org.glassfish.jersey.server.ServerProperties#TRACING_THRESHOLD
	 */
	private static final String PARAM_REST_TRACING_THRESHOLD = "restTracingThreshold"; //$NON-NLS-1$

	private static RestfulStrolchComponent instance;

	private String restTracing;
	private String restTracingThreshold;
	private boolean corsEnabled;
	private String corsOrigin;
	private boolean restLogging;
	private boolean restLoggingEntity;
	private String cacheMode;
	private boolean secureCookie;
	private int cookieMaxAge;

	public RestfulStrolchComponent(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	/**
	 * @return the corsEnabled
	 */
	public boolean isCorsEnabled() {
		return this.corsEnabled;
	}

	/**
	 * @return the origin
	 */
	public String getCorsOrigin() {
		return this.corsOrigin;
	}

	/**
	 * @return the restTracing
	 */
	public String getRestTracing() {
		return this.restTracing;
	}

	/**
	 * @return the restTracingThreshold
	 */
	public String getRestTracingThreshold() {
		return this.restTracingThreshold;
	}

	/**
	 * @return the restLogging
	 */
	public boolean isRestLogging() {
		return this.restLogging;
	}

	/**
	 * @return the restLoggingEntity
	 */
	public boolean isRestLoggingEntity() {
		return this.restLoggingEntity;
	}

	/**
	 * @return the secureCookie
	 */
	public boolean isSecureCookie() {
		return this.secureCookie;
	}

	/**
	 * @return the secureCookie
	 */
	public int getCookieMaxAge() {
		return this.cookieMaxAge;
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.corsEnabled = configuration.getBoolean(PARAM_CORS_ENABLED, Boolean.FALSE);
		if (this.corsEnabled) {
			this.corsOrigin = configuration.getString(PARAM_CORS_ORIGIN, null);
			logger.info("Enabling CORS for origin: " + this.corsOrigin); //$NON-NLS-1$
			AccessControlResponseFilter.setCorsEnabled(true);
			AccessControlResponseFilter.setOrigin(this.corsOrigin);
		}

		// restful logging and tracing
		this.restLogging = configuration.getBoolean(PARAM_REST_LOGGING, Boolean.FALSE);
		this.restLoggingEntity = configuration.getBoolean(PARAM_REST_LOGGING_ENTITY, Boolean.FALSE);
		this.restTracing = configuration.getString(PARAM_REST_TRACING, "OFF"); //$NON-NLS-1$
		this.restTracingThreshold = configuration.getString(PARAM_REST_TRACING_THRESHOLD, "TRACE"); //$NON-NLS-1$

		String msg = "Set restLogging={0} with logEntities={1} restTracing={2} with threshold={3}"; //$NON-NLS-1$
		logger.info(MessageFormat
				.format(msg, this.restLogging, this.restLoggingEntity, this.restTracing, this.restTracingThreshold));

		// set http cache mode
		this.cacheMode = configuration.getString(PARAM_HTTP_CACHE_MODE, HttpCacheResponseFilter.NO_CACHE);
		logger.info("HTTP header cache mode is set to {}", cacheMode);

		this.secureCookie = configuration.getBoolean(PARAM_SECURE_COOKIE, true);
		this.cookieMaxAge = configuration.getInt(PARAM_COOKIE_MAX_AGE, (int) TimeUnit.DAYS.toSeconds(1));

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {
		DBC.PRE.assertNull("Instance is already set! This component is a singleton resource!", instance); //$NON-NLS-1$
		instance = this;
		super.start();
	}

	@Override
	public void stop() throws Exception {
		instance = null;
		super.stop();
	}

	/**
	 * @return the RestfulStrolchComponent
	 */
	public static RestfulStrolchComponent getInstance() {
		DBC.PRE.assertNotNull("Not yet initialized!", instance); //$NON-NLS-1$
		return instance;
	}

	@Override
	public ComponentContainer getContainer() {
		return super.getContainer();
	}

	public StrolchAgent getAgent() {
		return super.getContainer().getAgent();
	}

	public <T> T getComponent(Class<T> clazz) {
		return getContainer().getComponent(clazz);
	}

	public StrolchSessionHandler getSessionHandler() {
		return getContainer().getComponent(StrolchSessionHandler.class);
	}

	public ServiceHandler getServiceHandler() {
		return getContainer().getComponent(ServiceHandler.class);
	}

	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz) {
		return getContainer().getRealm(certificate).openTx(certificate, clazz, true);
	}

	public StrolchTransaction openTx(Certificate certificate, String name) {
		return getContainer().getRealm(certificate).openTx(certificate, name, true);
	}

	public StrolchTransaction openTx(Certificate certificate, String realm, Class<?> clazz) {
		return getContainer().getRealm(realm).openTx(certificate, clazz, true);
	}

	public StrolchTransaction openTx(Certificate certificate, String realm, String name) {
		return getContainer().getRealm(realm).openTx(certificate, name, true);
	}
}
