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

import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import java.text.MessageFormat;
import java.util.ResourceBundle;

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
	public <T extends ServiceArgument, U extends ServiceResult> U doService(Certificate certificate,
			Service<T, U> service, T argument) {
		DBC.PRE.assertNotNull("Certificate my not be null!", certificate);

		long start = System.nanoTime();

		// first check that the caller may perform this service
		PrivilegeContext privilegeContext;
		String username = certificate.getUsername();
		try {
			privilegeContext = this.privilegeHandler.validate(certificate);
			privilegeContext.validateAction(service);
		} catch (PrivilegeException e) {

			long end = System.nanoTime();
			String msg = "User {0}: Service {1} failed after {2} due to {3}"; //$NON-NLS-1$
			String svcName = service.getClass().getName();
			msg = MessageFormat.format(msg, username, svcName, formatNanoDuration(end - start), e.getMessage());
			logger.error(msg);

			if (getContainer().hasComponent(OperationsLog.class)) {
				String realmName = getRealmName(argument, certificate);
				LogMessage logMessage = new LogMessage(realmName, username,
						Locator.valueOf(AGENT, PrivilegeHandler.class.getSimpleName(), service.getPrivilegeName(),
								svcName), LogSeverity.Exception, LogMessageState.Information,
						ResourceBundle.getBundle("strolch-agent"), "agent.service.failed.access.denied")
						.value("user", username).value("service", svcName).withException(e);

				OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
				operationsLog.addMessage(logMessage);
			}

			I18nMessage i18n = new I18nMessage(ResourceBundle.getBundle("strolch-agent", certificate.getLocale()),
					"agent.service.failed.access.denied").value("user", username)
					.value("service", service.getClass().getSimpleName());

			if (!this.throwOnPrivilegeFail && service instanceof AbstractService) {
				logger.error(e.getMessage(), e);

				AbstractService<?, ?> abstractService = (AbstractService<?, ?>) service;
				@SuppressWarnings("unchecked")
				U result = (U) abstractService.getResultInstance();
				result.setState(e instanceof PrivilegeModelException ?
						ServiceResultState.EXCEPTION :
						ServiceResultState.ACCESS_DENIED);
				result.setMessage(i18n.getMessage());
				result.i18n(i18n);
				result.setThrowable(e);
				return result;
			}

			if (e instanceof PrivilegeModelException)
				throw new StrolchException(i18n.getMessage(), e);
			throw new StrolchAccessDeniedException(certificate, service, i18n, e);
		}

		try {
			// then perform the service
			if (service instanceof AbstractService) {
				AbstractService<?, ?> abstractService = (AbstractService<?, ?>) service;
				abstractService.setContainer(getContainer());
				abstractService.setPrivilegeContext(privilegeContext);
			}

			U serviceResult = service.doService(argument);
			if (serviceResult == null) {
				String msg = "Service {0} is not properly implemented as it returned a null result!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, service.getClass().getSimpleName());
				throw new StrolchException(msg);
			}

			// log the result
			logResult(service, argument, start, certificate, serviceResult);

			return serviceResult;

		} catch (Exception e) {
			long end = System.nanoTime();
			String msg = "User {0}: Service failed {1} after {2} due to {3}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, service.getClass().getName(), formatNanoDuration(end - start),
					e.getMessage());
			logger.error(msg);
			throw new StrolchException(msg, e);
		}
	}

	private String getRealmName(ServiceArgument arg, Certificate certificate) {
		if (arg == null) {
			return isNotEmpty(certificate.getRealm()) ?
					certificate.getRealm() :
					getContainer().getRealmNames().iterator().next();
		}

		if (isNotEmpty(arg.realm))
			return arg.realm;

		if (isNotEmpty(certificate.getRealm()))
			return certificate.getRealm();

		return getContainer().getRealmNames().iterator().next();
	}

	private void logResult(Service<?, ?> service, ServiceArgument arg, long start, Certificate certificate,
			ServiceResult serviceResult) {

		long end = System.nanoTime();

		String msg = "User {0}: Service {1} took {2}"; //$NON-NLS-1$
		String username = certificate.getUsername();
		String svcName = service.getClass().getName();

		String realmName = getRealmName(arg, certificate);

		msg = MessageFormat.format(msg, username, svcName, formatNanoDuration(end - start));

		if (serviceResult.getState() == ServiceResultState.SUCCESS) {
			logger.info(msg);
		} else if (serviceResult.getState() == ServiceResultState.WARNING) {

			msg = ServiceResultState.WARNING + ": " + msg;
			logger.warn(msg);

			if (isNotEmpty(serviceResult.getMessage()) && serviceResult.getThrowable() != null) {
				logger.warn("Reason: " + serviceResult.getMessage(), serviceResult.getThrowable());
			} else if (isNotEmpty(serviceResult.getMessage())) {
				logger.warn("Reason: " + serviceResult.getMessage());
			} else if (serviceResult.getThrowable() != null) {
				logger.warn("Reason: " + serviceResult.getThrowable().getMessage(), serviceResult.getThrowable());
			}

		} else if (serviceResult.getState() == ServiceResultState.FAILED
				|| serviceResult.getState() == ServiceResultState.EXCEPTION
				|| serviceResult.getState() == ServiceResultState.ACCESS_DENIED) {

			msg = serviceResult.getState() + ": " + msg;
			logger.error(msg);

			String reason = null;
			Throwable throwable = null;
			if (isNotEmpty(serviceResult.getMessage()) && serviceResult.getThrowable() != null) {
				reason = serviceResult.getMessage();
				throwable = serviceResult.getThrowable();
			} else if (isNotEmpty(serviceResult.getMessage())) {
				reason = serviceResult.getMessage();
			} else if (serviceResult.getThrowable() != null) {
				reason = serviceResult.getThrowable().getMessage();
				throwable = serviceResult.getThrowable();
			}

			if (throwable == null)
				logger.error("Reason: " + reason);
			else
				logger.error("Reason: " + reason, throwable);

			if ((serviceResult.getState() == ServiceResultState.EXCEPTION
					|| serviceResult.getState() == ServiceResultState.ACCESS_DENIED) //
					&& getContainer().hasComponent(OperationsLog.class)) {

				LogMessage logMessage;

				ResourceBundle bundle = ResourceBundle.getBundle("strolch-agent");
				if (throwable == null) {
					logMessage = new LogMessage(realmName, username, Locator.valueOf(AGENT, svcName, getUniqueId()),
							LogSeverity.Exception, LogMessageState.Information, bundle, "agent.service.failed")
							.value("service", svcName).value("reason", reason);
				} else {
					logMessage = new LogMessage(realmName, username, Locator.valueOf(AGENT, svcName, getUniqueId()),
							LogSeverity.Exception, LogMessageState.Information, bundle, "agent.service.failed.ex")
							.withException(throwable).value("service", svcName).value("reason", reason)
							.value("exception", throwable);
				}

				OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
				operationsLog.addMessage(logMessage);
			}

		} else if (serviceResult.getState() == null) {
			logger.error("Service " + svcName + " returned a null ServiceResultState!");
			logger.error(msg);
		} else {
			logger.error("UNHANDLED SERVICE RESULT STATE: " + serviceResult.getState());
			logger.error(msg);
		}
	}
}
