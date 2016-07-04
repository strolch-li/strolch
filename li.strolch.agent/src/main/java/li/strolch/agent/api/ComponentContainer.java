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
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ComponentContainer {

	public abstract StrolchAgent getAgent();

	public abstract ComponentState getState();

	public abstract boolean hasComponent(Class<?> clazz);

	public abstract <T> T getComponent(Class<T> clazz) throws IllegalArgumentException;

	public abstract PrivilegeHandler getPrivilegeHandler() throws IllegalArgumentException;

	public abstract Set<Class<?>> getComponentTypes();

	public abstract Set<String> getRealmNames();

	/**
	 * Returns the {@link StrolchRealm} with the given name. To get the default realm, use the constant
	 * {@link StrolchConstants#DEFAULT_REALM}.
	 * 
	 * @param realm
	 *            the name of the {@link StrolchRealm} to return
	 * 
	 * @return the {@link StrolchRealm} with the given name
	 * 
	 * @throws StrolchException
	 *             if the {@link StrolchRealm} does not exist with the given name
	 */
	public abstract StrolchRealm getRealm(String realm) throws StrolchException;

	/**
	 * Returns the default {@link StrolchRealm} for the user with the given {@link Certificate}. This is done by
	 * querying the property {@link StrolchConstants#PROP_REALM} from the certificate.
	 * 
	 * @param certificate
	 *            the {@link Certificate} from which to retrieve the name of the {@link StrolchRealm} to return
	 * 
	 * @return the {@link StrolchRealm}
	 * 
	 * @throws StrolchException
	 *             if the user does not have a {@link StrolchConstants#PROP_REALM} property configured, and the default
	 *             realm is not configured, or if the realm does not exist with the found value
	 */
	public abstract StrolchRealm getRealm(Certificate certificate) throws StrolchException;

}