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
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultServiceHandler extends StrolchComponent implements ServiceHandler {

	private RuntimeConfiguration runtimeConfiguration;
	private PrivilegeHandler privilegeHandler;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultServiceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		this.privilegeHandler = getContainer().getPrivilegeHandler();
		this.runtimeConfiguration = configuration.getRuntimeConfiguration();
		super.initialize(configuration);
	}

	public <T> T getComponent(Class<T> clazz) {
		return getContainer().getComponent(clazz);
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
		PrivilegeContext privilegeContext = this.privilegeHandler.getPrivilegeContext(certificate);
		privilegeContext.validateAction(service);

		try {
			// then perform the service
			if (service instanceof AbstractService) {
				AbstractService<?, ?> abstractService = (AbstractService<?, ?>) service;
				abstractService.setContainer(getContainer());
				if (privilegeContext != null)
					abstractService.setPrivilegeContext(privilegeContext);
			}

			U serviceResult = service.doService(argument);
			if (serviceResult == null) {
				String msg = "Service {0} is not properly implemented as it returned a null result!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, service);
				throw new StrolchException(msg);
			}

			// log the result
			long end = System.nanoTime();
			String msg = "Service {0} took {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, service, StringHelper.formatNanoDuration(end - start));
			if (serviceResult.getState() == ServiceResultState.SUCCESS)
				logger.info(msg);
			else if (serviceResult.getState() == ServiceResultState.WARNING)
				logger.warn(msg);
			else if (serviceResult.getState() == ServiceResultState.FAILED)
				logger.error(msg);

			return serviceResult;

		} catch (Exception e) {
			long end = System.nanoTime();
			String msg = "Failed to perform service {0} after {1} due to {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, service, StringHelper.formatNanoDuration(end - start), e.getMessage());
			logger.error(msg, e);
			throw new StrolchException(msg, e);
		}
	}
}
