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
package li.strolch.agent.api;

import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;

/**
 * Strolch's Container for all its components
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ComponentContainer {

	StrolchAgent getAgent();

	ComponentState getState();

	boolean hasComponent(Class<?> clazz);

	/**
	 * Returns the reference to the {@link StrolchComponent} with the given name, if it exists. If it does not exist, an
	 * {@link IllegalArgumentException} is thrown
	 *
	 * @param clazz
	 * 		the type of component to return
	 *
	 * @return the component with the given name
	 *
	 * @throws IllegalArgumentException
	 * 		if the component does not exist
	 */
	<T> T getComponent(Class<T> clazz) throws IllegalArgumentException;

	PrivilegeHandler getPrivilegeHandler() throws IllegalArgumentException;

	Set<Class<?>> getComponentTypes();

	Set<String> getRealmNames();

	/**
	 * Returns the {@link StrolchRealm} with the given name. To get the default realm, use the constant {@link
	 * StrolchConstants#DEFAULT_REALM}.
	 *
	 * @param realm
	 * 		the name of the {@link StrolchRealm} to return
	 *
	 * @return the {@link StrolchRealm} with the given name
	 *
	 * @throws StrolchException
	 * 		if the {@link StrolchRealm} does not exist with the given name
	 */
	StrolchRealm getRealm(String realm) throws StrolchException;

	/**
	 * Returns the default {@link StrolchRealm} for the user with the given {@link Certificate}. This is done by
	 * querying the property {@link StrolchConstants#PROP_REALM} from the certificate.
	 *
	 * @param certificate
	 * 		the {@link Certificate} from which to retrieve the name of the {@link StrolchRealm} to return
	 *
	 * @return the {@link StrolchRealm}
	 *
	 * @throws StrolchException
	 * 		if the user does not have a {@link StrolchConstants#PROP_REALM} property configured, and the default realm is
	 * 		not configured, or if the realm does not exist with the found value
	 */
	StrolchRealm getRealm(Certificate certificate) throws StrolchException;

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception;

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	<T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws PrivilegeException, Exception;
}