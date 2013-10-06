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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import ch.eitchnet.xmlpers.api.ParserFactory;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.test.impl.Book;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TestPersistenceContextFactory implements PersistenceContextFactory {

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(String type, String subType, String id) {
		return buildPersistenceContext(type, subType, id);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(String type, String subType) {
		return buildPersistenceContext(type, subType, null);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(String type) {
		return buildPersistenceContext(type, null, null);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(T t) {
		if (t == null)
			throw new RuntimeException("T may not be null!");

		PersistenceContext<T> context;

		if (t instanceof Resource) {
			Resource resource = (Resource) t;
			context = buildPersistenceContext(TestConstants.TYPE_RES, resource.getType(), resource.getId());
		} else if (t instanceof Book) {
			context = buildPersistenceContext(TestConstants.TYPE_BOOK, null, ((Book) t).getId().toString());
		} else {
			throw new UnsupportedOperationException("Handling of " + t.getClass().getName() + " is not implemented!");
		}

		context.setObject(t);
		return context;
	}

	private <T> PersistenceContext<T> buildPersistenceContext(String type, String subType, String id) {

		PersistenceContext<T> context = new PersistenceContext<>();

		context.setType(type);
		context.setSubType(subType);
		context.setId(id);

		context.setParserFactory(this.<T> getParserFactoryInstance(type));

		return context;
	}

	/**
	 * @param type
	 * @return
	 */
	@SuppressWarnings("unchecked")
	private <T> ParserFactory<T> getParserFactoryInstance(String type) {

		ParserFactory<T> parserFactory;
		if (type.equals(TestConstants.TYPE_RES))
			parserFactory = (ParserFactory<T>) new ResourceParserFactory();
		else if (type.equals(TestConstants.TYPE_BOOK))
			parserFactory = (ParserFactory<T>) new BookParserFactory();
		else
			throw new UnsupportedOperationException("No ParserFactory can be returned for type " + type);

		return parserFactory;
	}
}
