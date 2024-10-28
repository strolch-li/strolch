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
package li.strolch.service.api;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.ResourceBundle;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.service.api.ServiceResultState.*;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultServiceHandler extends StrolchComponent implements ServiceHandler {

	private static final String PARAM_THROW_ON_PRIVILEGE_FAIL = "throwOnPrivilegeFail";
	private RuntimeConfiguration runtimeConfiguration;
	private PrivilegeHandler privilegeHandler;
	private boolean throwOnPrivilegeFail;

	public DefaultServiceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.privilegeHandler = getContainer().getPrivilegeHandler();
		this.runtimeConfiguration = configuration.getRuntimeConfiguration();
		this.throwOnPrivilegeFail = configuration.getBoolean(PARAM_THROW_ON_PRIVILEGE_FAIL, Boolean.FALSE);
		super.initialize(configuration);
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.runtimeConfiguration;
	}

	@Override
	public <U extends ServiceResult> U doService(Certificate certificate, Service<ServiceArgument, U> service) {
		return doService(certificate, service, null);
	}

	@Override
	public <T extends ServiceArgument, U extends ServiceResult> U doService(Certificate certificate, Service<T, U> svc,
			T argument) {
		DBC.PRE.assertNotNull("Certificate my not be null!", certificate);

		if (!(svc instanceof AbstractService<T, U> service))
			throw new IllegalArgumentException(
					"This service handle expects all services to be instance of " + AbstractService.class.getName());

		long start = System.nanoTime();

		// first check that the caller may perform this service
		PrivilegeContext privilegeContext;
		String username = certificate.getUsername();
		try {
			privilegeContext = this.privilegeHandler.validate(certificate);
			privilegeContext.validateAction(service);
		} catch (PrivilegeModelException e) {
			I18nMessage i18n = logAccessDenied(certificate, argument, service, start, username, e);
			if (this.throwOnPrivilegeFail)
				throw new StrolchException(i18n.getMessage(), e);
			return buildFailedResult(service, e, i18n);
		} catch (PrivilegeException e) {
			I18nMessage i18n = logAccessDenied(certificate, argument, service, start, username, e);
			if (this.throwOnPrivilegeFail)
				throw new StrolchAccessDeniedException(certificate, service, i18n, e);
			return buildFailedResult(service, e, i18n);
		}

		try {
			// then perform the service
			service.setContainer(getContainer());
			service.setPrivilegeContext(privilegeContext);
			U result = service.doService(argument);

			// log the result
			logResult(service, argument, start, certificate, result);
			return result;

		} catch (Exception e) {
			long end = System.nanoTime();
			String msg = "User {0}: Service failed {1} after {2} due to {3}";
			String reason = getRootCauseMessage(e);
			msg = MessageFormat.format(msg, username, service.getClass().getName(), formatNanoDuration(end - start),
					reason);

			ResourceBundle bundle = ResourceBundle.getBundle("strolch-agent");
			LogMessage logMessage = new LogMessage(getRealmName(argument, certificate), username,
					Locator.valueOf(AGENT, Service.class.getSimpleName(), service.getClass().getName()),
					LogSeverity.Exception, LogMessageState.Information, bundle, "agent.service.failed.ex")
					.value("service", service.getClass().getName())
					.value("reason", reason)
					.value("exception", e);

			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			operationsLog.addMessage(logMessage);

			logger.error(msg);
			throw new StrolchException(msg, e);
		}
	}

	private <T extends ServiceArgument, U extends ServiceResult> I18nMessage logAccessDenied(Certificate certificate,
			T argument, AbstractService<T, U> service, long start, String username, PrivilegeException e) {
		logger.error(buildFailMessage(service, start, username, e));
		addAccessDeniedLogMessage(certificate, argument, username, e, service.getClass().getName());
		I18nMessage i18n = buildAccessDeniedMessage(certificate, service, username);
		logger.error(e.getMessage(), e);
		return i18n;
	}

	private static <T extends ServiceArgument, U extends ServiceResult> U buildFailedResult(
			AbstractService<T, U> service, PrivilegeException e, I18nMessage i18n) {
		U result = service.getResultInstance();
		result.setState(e instanceof PrivilegeModelException ? EXCEPTION : ACCESS_DENIED);
		result.setMessage(i18n.getMessage());
		result.i18n(i18n);
		result.setThrowable(e);
		return result;
	}

	private static <T extends ServiceArgument, U extends ServiceResult> I18nMessage buildAccessDeniedMessage(
			Certificate certificate, AbstractService<T, U> service, String username) {
		return new I18nMessage(ResourceBundle.getBundle("strolch-agent", certificate.getLocale()),
				"agent.service.failed.access.denied")
				.value("user", username)
				.value("service", service.getClass().getSimpleName());
	}

	private static <T extends ServiceArgument, U extends ServiceResult> String buildFailMessage(
			AbstractService<T, U> service, long start, String username, PrivilegeException e) {
		long end = System.nanoTime();
		String msg = "User {0}: Service {1} failed after {2} due to {3}";
		msg = MessageFormat.format(msg, username, service.getClass().getName(), formatNanoDuration(end - start),
				e.getMessage());
		return msg;
	}

	private <T extends ServiceArgument> void addAccessDeniedLogMessage(Certificate certificate, T argument,
			String username, PrivilegeException e, String svcName) {
		if (getContainer().hasComponent(OperationsLog.class)) {
			String realmName = getRealmName(argument, certificate);
			LogMessage logMessage = new LogMessage(realmName, username,
					Locator.valueOf(AGENT, Service.class.getSimpleName(), svcName), LogSeverity.Exception,
					LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
					"agent.service.failed.access.denied")
					.value("user", username)
					.value("service", svcName)
					.withException(e);

			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			operationsLog.addMessage(logMessage);
		}
	}

	private String getRealmName(ServiceArgument arg, Certificate certificate) {
		if (arg == null) {
			return isNotEmpty(certificate.getRealm()) ? certificate.getRealm() :
					getContainer().getRealmNames().iterator().next();
		}

		if (isNotEmpty(arg.realm))
			return arg.realm;

		if (isNotEmpty(certificate.getRealm()))
			return certificate.getRealm();

		return getContainer().getRealmNames().iterator().next();
	}

	private void logResult(Service<?, ?> service, ServiceArgument arg, long start, Certificate certificate,
			ServiceResult result) {

		long end = System.nanoTime();

		String msg = "User {0}: Service {1} took {2}";
		String username = certificate.getUsername();
		String svcName = service.getClass().getName();

		String realmName = getRealmName(arg, certificate);

		long durationNanos = end - start;
		msg = MessageFormat.format(msg, username, svcName, formatNanoDuration(durationNanos));

		if (result.getState() == SUCCESS) {
			logger.info(msg);
		} else if (result.getState() == WARNING) {

			msg = WARNING + ": " + msg;
			logger.warn(msg);

			if (isNotEmpty(result.getMessage()) && result.getThrowable() != null) {
				logger.warn("Reason: {}", result.getMessage(), result.getThrowable());
			} else if (isNotEmpty(result.getMessage())) {
				logger.warn("Reason: {}", result.getMessage());
			} else if (result.getThrowable() != null) {
				logger.warn("Reason: {}", getRootCauseMessage(result.getThrowable()), result.getThrowable());
			}

		} else if (result.getState() == FAILED
				|| result.getState() == EXCEPTION
				|| result.getState() == ACCESS_DENIED) {

			msg = result.getState() + ": " + msg;
			logger.error(msg);

			String reason = null;
			Throwable throwable = null;
			if (isNotEmpty(result.getMessage()) && result.getThrowable() != null) {
				reason = result.getMessage();
				throwable = result.getThrowable();
			} else if (isNotEmpty(result.getMessage())) {
				reason = result.getMessage();
			} else if (result.getThrowable() != null) {
				reason = getRootCauseMessage(result.getThrowable());
				throwable = result.getThrowable();
			}

			if (throwable == null)
				logger.error("Reason: {}", reason);
			else
				logger.error("Reason: {}", reason, throwable);

			if ((result.getState() == EXCEPTION || result.getState() == ACCESS_DENIED) //
					&& getContainer().hasComponent(OperationsLog.class)) {

				LogMessage logMessage;

				ResourceBundle bundle = ResourceBundle.getBundle("strolch-agent");
				if (throwable == null) {
					logMessage = new LogMessage(realmName, username,
							Locator.valueOf(AGENT, Service.class.getSimpleName(), svcName), LogSeverity.Exception,
							LogMessageState.Information, bundle, "agent.service.failed")
							.value("service", svcName)
							.value("reason", reason);
				} else {
					logMessage = new LogMessage(realmName, username,
							Locator.valueOf(AGENT, Service.class.getSimpleName(), svcName), LogSeverity.Exception,
							LogMessageState.Information, bundle, "agent.service.failed.ex")
							.value("service", svcName)
							.value("reason", reason)
							.value("exception", throwable);
				}

				OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
				operationsLog.addMessage(logMessage);
			}

		} else if (result.getState() == null) {
			logger.error("Service {} returned a null ServiceResultState!", svcName);
			logger.error(msg);
		} else {
			logger.error("UNHANDLED SERVICE RESULT STATE: {}", result.getState());
			logger.error(msg);
		}

		// record the event
		getAgent().getAgentStatistics().recordService(durationNanos);
	}
}
