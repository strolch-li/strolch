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

import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultServiceHandler extends StrolchComponent implements ServiceHandler {

	private static final String PARAM_THROW_ON_PRIVILEGE_FAIL = "throwOnPrivilegeFail";
	private RuntimeConfiguration runtimeConfiguration;
	private PrivilegeHandler privilegeHandler;
	private boolean throwOnPrivilegeFail;

	/**
	 * @param container
	 * @param componentName
	 */
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

		long start = System.nanoTime();

		// first check that the caller may perform this service
		PrivilegeContext privilegeContext;
		String username = certificate == null ? "null" : certificate.getUsername();
		try {
			privilegeContext = this.privilegeHandler.getPrivilegeContext(certificate);
			privilegeContext.validateAction(service);
		} catch (PrivilegeException e) {

			long end = System.nanoTime();
			String msg = "User {0}: Service {1} failed after {2} due to {3}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, service.getClass().getName(),
					StringHelper.formatNanoDuration(end - start), e.getMessage());
			logger.error(msg);

			if (!this.throwOnPrivilegeFail && service instanceof AbstractService) {
				logger.error(e.getMessage(), e);

				AbstractService<?, ?> abstractService = (AbstractService<?, ?>) service;
				@SuppressWarnings("unchecked")
				U arg = (U) abstractService.getResultInstance();
				arg.setState(ServiceResultState.ACCESS_DENIED);
				arg.setMessage(e.getMessage());
				arg.setThrowable(e);
				return arg;
			}

			throw new StrolchAccessDeniedException(certificate, service, e.getMessage(), e);
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
			logResult(service, start, username, serviceResult);

			return serviceResult;

		} catch (Exception e) {
			long end = System.nanoTime();
			String msg = "User {0}: Service failed {1} after {2} due to {3}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, service.getClass().getName(),
					StringHelper.formatNanoDuration(end - start), e.getMessage());
			logger.error(msg);
			throw new StrolchException(msg, e);
		}
	}

	private void logResult(Service<?, ?> service, long start, String username, ServiceResult serviceResult) {

		long end = System.nanoTime();

		String msg = "User {0}: Service {1} took {2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, username, service.getClass().getName(),
				StringHelper.formatNanoDuration(end - start));

		if (serviceResult.getState() == ServiceResultState.SUCCESS) {
			logger.info(msg);
		} else if (serviceResult.getState() == ServiceResultState.WARNING) {

			msg = ServiceResultState.WARNING + ": " + msg;
			logger.warn(msg);

			if (StringHelper.isNotEmpty(serviceResult.getMessage()) && serviceResult.getThrowable() != null) {
				logger.warn("Reason: " + serviceResult.getMessage(), serviceResult.getThrowable());
			} else if (StringHelper.isNotEmpty(serviceResult.getMessage())) {
				logger.warn("Reason: " + serviceResult.getMessage());
			} else if (serviceResult.getThrowable() != null) {
				logger.warn("Reason: " + serviceResult.getThrowable().getMessage(), serviceResult.getThrowable());
			}

		} else if (serviceResult.getState() == ServiceResultState.FAILED
				|| serviceResult.getState() == ServiceResultState.ACCESS_DENIED) {

			msg = serviceResult.getState() + ": " + msg;
			logger.error(msg);

			if (StringHelper.isNotEmpty(serviceResult.getMessage()) && serviceResult.getThrowable() != null) {
				logger.error("Reason: " + serviceResult.getMessage(), serviceResult.getThrowable());
			} else if (StringHelper.isNotEmpty(serviceResult.getMessage())) {
				logger.error("Reason: " + serviceResult.getMessage());
			} else if (serviceResult.getThrowable() != null) {
				logger.error("Reason: " + serviceResult.getThrowable().getMessage(), serviceResult.getThrowable());
			}
		} else if (serviceResult.getState() == null) {
			logger.error("Service " + service.getClass().getName() + " returned a null ServiceResultState!");
			logger.error(msg);
		} else {
			logger.error("UNHANDLED SERVICE RESULT STATE: " + serviceResult.getState());
			logger.error(msg);
		}
	}
}
