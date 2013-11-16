/*
 * Copyright (c) 2012
 *
 * This file is part of ???????????????
 *
 * ?????????????? is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ????????????????.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package li.strolch.service;

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractService<T extends ServiceArgument, U extends ServiceResult> implements Service<T, U> {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractService.class);
	private static final long serialVersionUID = 1L;

	@Override
	public final U doService(T argument) {

		if (isArgumentRequired() && argument == null) {

			String msg = "Failed to perform service {0} because no argument was passed although it is required!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getClass());
			logger.error(msg);

			U result = getResultInstance();
			result.setState(ServiceResultState.FAILED);
			result.setMessage(msg);
			return result;
		}

		try {

			return internalDoService(argument);

		} catch (Exception e) {

			String msg = "Failed to perform service {0} due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getClass(), e.getMessage());
			logger.error(msg, e);

			U result = getResultInstance();
			result.setState(ServiceResultState.FAILED);
			result.setMessage(msg);
			result.setThrowable(e);
			return result;
		}
	}

	/**
	 * @return if true, then an argument must be set to execute the service. If the argument is missing, then the
	 *         service execution fails immediately
	 */
	protected boolean isArgumentRequired() {
		return true;
	}

	/**
	 * This method is called if the service execution fails and an instance of the expected {@link ServiceResult} is
	 * required to return to the caller
	 * 
	 * @return
	 */
	protected abstract U getResultInstance();

	/**
	 * Internal method to perform the {@link Service}. The implementor does not need to handle exceptions as this is
	 * done in the {@link #doService(ServiceArgument)} which calls this method
	 * 
	 * @param argument
	 * @return
	 */
	protected abstract U internalDoService(T argument);

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return Service.class.getName();
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}
}
