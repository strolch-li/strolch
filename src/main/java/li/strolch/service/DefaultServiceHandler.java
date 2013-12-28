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
package li.strolch.service;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.runtime.agent.api.StrolchComponent;
import li.strolch.runtime.agent.impl.ComponentContainerImpl;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.StrolchPrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultServiceHandler extends StrolchComponent implements ServiceHandler {

	private StrolchPrivilegeHandler privilegeHandler;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultServiceHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		if (getContainer().hasComponent(StrolchPrivilegeHandler.class))
			this.privilegeHandler = getContainer().getComponent(StrolchPrivilegeHandler.class);
		super.initialize(configuration);
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
		if (this.privilegeHandler != null) {
			// XXX Bad bad bad: remove the need for thread locals...
			try {
				PrivilegeContext privilegeContext = this.privilegeHandler.getPrivilegeContext(certificate);
				PrivilegeContext.set(privilegeContext);
				privilegeContext.validateAction(service);
			} finally {
				PrivilegeContext.set(null);
			}
		}

		try {
			// then perform the service
			service.setContainer(getContainer());
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
