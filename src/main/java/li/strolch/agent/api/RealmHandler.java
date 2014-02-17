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

import li.strolch.agent.impl.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface RealmHandler {

	/**
	 * Returns the names of the configured {@link StrolchRealm StrolchRealms}
	 * 
	 * @return the set of names of the configured {@link StrolchRealm StrolchRealms}
	 */
	public Set<String> getRealmNames();

	/**
	 * Returns the {@link StrolchRealm} with the given name. To get the default realm, use the constante
	 * {@link StrolchConstants#DEFAULT_REALM}.
	 * 
	 * @param realm
	 *            the name of the {@link StrolchRealm} to return
	 * @return the {@link StrolchRealm} with the given name
	 * 
	 * @throws StrolchException
	 *             if the {@link StrolchRealm} does not exist with the given name
	 */
	public StrolchRealm getRealm(String realm);
}
