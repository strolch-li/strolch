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
package li.strolch.xmlpers.api;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PersistenceContextFactoryDelegator {

	private Map<String, PersistenceContextFactory<?>> contextFactoryCacheByType;
	private Map<Class<?>, PersistenceContextFactory<?>> contextFactoryCacheByClass;

	public PersistenceContextFactoryDelegator() {
		this.contextFactoryCacheByType = new HashMap<>();
		this.contextFactoryCacheByClass = new HashMap<>();
	}

	public void registerPersistenceContextFactory(Class<?> classType, String type,
			PersistenceContextFactory<?> ctxFactory) {

		this.contextFactoryCacheByClass.put(classType, ctxFactory);
		this.contextFactoryCacheByType.put(type, ctxFactory);
	}

	public <T> PersistenceContextFactory<T> getCtxFactory(Class<?> classType) {

		@SuppressWarnings("unchecked")
		PersistenceContextFactory<T> ctxFactory = (PersistenceContextFactory<T>) this.contextFactoryCacheByClass
				.get(classType);
		if (ctxFactory != null)
			return ctxFactory;

		String msg = "No context factory is registered for {0}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, classType);
		throw new IllegalArgumentException(msg);
	}

	public <T> PersistenceContextFactory<T> getCtxFactory(String type) {

		@SuppressWarnings("unchecked")
		PersistenceContextFactory<T> ctxFactory = (PersistenceContextFactory<T>) this.contextFactoryCacheByType
				.get(type);
		if (ctxFactory != null)
			return ctxFactory;

		String msg = "No context factory is registered for type {0}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, type);
		throw new IllegalArgumentException(msg);
	}
}
