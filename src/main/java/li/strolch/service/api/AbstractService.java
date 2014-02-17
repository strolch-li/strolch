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
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.RuntimeConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractService<T extends ServiceArgument, U extends ServiceResult> implements Service<T, U> {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractService.class);
	private static final long serialVersionUID = 1L;

	private ComponentContainer container;
	private Certificate certificate;

	/**
	 * @param container
	 *            the container to set
	 */
	public void setContainer(ComponentContainer container) {
		this.container = container;
	}

	/**
	 * @param certificate
	 *            the certificate to set
	 */
	public void setCertificate(Certificate certificate) {
		this.certificate = certificate;
	}

	/**
	 * @return the certificate
	 */
	protected Certificate getCertificate() {
		return this.certificate;
	}

	/**
	 * @return the container
	 */
	protected ComponentContainer getContainer() {
		return this.container;
	}

	protected <V> V getComponent(Class<V> clazz) {
		return this.container.getComponent(clazz);
	}

	protected RuntimeConfiguration getRuntimeConfiguration() {
		return this.container.getAgent().getStrolchConfiguration().getRuntimeConfiguration();
	}

	protected StrolchRealm getRealm(String realm) {
		return getComponent(RealmHandler.class).getRealm(realm);
	}

	protected StrolchTransaction openTx(String realm) {
		return getComponent(RealmHandler.class).getRealm(realm).openTx();
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
