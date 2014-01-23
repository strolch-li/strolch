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

import li.strolch.agent.impl.DataStoreMode;
import li.strolch.agent.impl.StrolchRealm;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ComponentContainer {

	public abstract StrolchAgent getAgent();

	public abstract ComponentState getState();

	public abstract DataStoreMode getDataStoreMode();

	public abstract boolean hasComponent(Class<?> clazz);

	public abstract <T> T getComponent(Class<T> clazz);
	
	public abstract Set<Class<?>> getComponentTypes();

	public abstract Set<String> getRealmNames();

	public abstract StrolchRealm getDefaultRealm();

	public abstract StrolchRealm getRealm(String realm);

}