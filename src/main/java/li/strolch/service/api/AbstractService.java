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
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.RuntimeConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractService<T extends ServiceArgument, U extends ServiceResult> implements Service<T, U> {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractService.class);
	private static final long serialVersionUID = 1L;

	private ComponentContainer container;
	private PrivilegeContext privilegeContext;

	/**
	 * @param privilegeContext
	 *            the privilegeContext to set
	 */
	public final void setPrivilegeContext(PrivilegeContext privilegeContext) {
		DBC.PRE.assertNull("PrivilegeContext is already set!", this.privilegeContext);
		this.privilegeContext = privilegeContext;
	}

	/**
	 * @return the privilegeContext
	 */
	public final PrivilegeContext getPrivilegeContext() {
		return this.privilegeContext;
	}

	/**
	 * @return the certificate
	 */
	protected final Certificate getCertificate() {
		return this.privilegeContext.getCertificate();
	}

	/**
	 * @param container
	 *            the container to set
	 */
	public final void setContainer(ComponentContainer container) {
		this.container = container;
	}

	/**
	 * @return the container
	 */
	protected final ComponentContainer getContainer() {
		return this.container;
	}

	/**
	 * @param clazz
	 * @return
	 */
	protected final <V> V getComponent(Class<V> clazz) {
		return this.container.getComponent(clazz);
	}

	/**
	 * @return
	 */
	protected final RuntimeConfiguration getRuntimeConfiguration() {
		return this.container.getAgent().getStrolchConfiguration().getRuntimeConfiguration();
	}

	/**
	 * @param realm
	 * @return
	 */
	protected final StrolchRealm getRealm(String realm) {
		return this.container.getRealm(realm);
	}

	/**
	 * @param realm
	 * @return
	 */
	protected final StrolchTransaction openTx(String realm) {
		return this.container.getRealm(realm).openTx();
	}

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

			U serviceResult = internalDoService(argument);
			if (serviceResult == null) {
				String msg = "Service {0} is not properly implemented as it returned a null result!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.getClass().getName());
				throw new StrolchException(msg);
			}

			return serviceResult;

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
	 * @param arg
	 * 
	 * @return a {@link ServiceResult} which denotes the execution state of this {@link Service}
	 * 
	 * @throws Exception
	 *             if something went wrong. The caller will catch and handle the {@link ServiceResult}
	 */
	protected abstract U internalDoService(T arg) throws Exception;

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
