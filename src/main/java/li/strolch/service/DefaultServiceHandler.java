/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.service;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
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
	public DefaultServiceHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
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
			U serviceResult = service.doService(argument);

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

	@Override
	public void initialize(ComponentConfiguration configuration) {
		if (getContainer().hasComponent(StrolchPrivilegeHandler.class))
			this.privilegeHandler = getContainer().getComponent(StrolchPrivilegeHandler.class);
		super.initialize(configuration);
	}
}
