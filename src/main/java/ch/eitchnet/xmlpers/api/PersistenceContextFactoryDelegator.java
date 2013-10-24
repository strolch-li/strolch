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
package ch.eitchnet.xmlpers.api;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
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
