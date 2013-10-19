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
package ch.eitchnet.xmlpers.test.model;

import org.junit.Assert;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class ModelBuilder {

	public static final String RES_TYPE = "@subType";
	public static final String RES_TYPE_INEXISTANT = "@inexistant";
	public static final String RES_NAME = "@name";
	public static final String RES_NAME_MODIFIED = "@name_modified";
	public static final String RES_ID = "@id";

	public static final String PARAM_TYPE = "@paramType";
	public static final String PARAM_NAME = "@paramName";
	public static final String PARAM_ID = "@paramId";
	public static final String PARAM_VALUE_1 = "@paramValue1";
	public static final String PARAM_VALUE_2 = "@paramValue2";

	public static final long BOOK_ID = 10L;
	public static final String BOOK_TITLE = "Nick Hornby";
	public static final String BOOK_AUTHOR = "A long way down";
	public static final String BOOK_PRESS_1 = "Some press";
	public static final String BOOK_PRESS_2 = "Another press";
	public static final double BOOK_PRICE = 45.55D;

	public static Resource createResource() {
		return createResource(RES_ID, RES_NAME, RES_TYPE);
	}
	
	public static Resource createResource(String id) {
		return createResource(id, RES_NAME, RES_TYPE);
	}

	public static Resource createResource(String id, String name, String type) {
		Resource resource = new Resource(id, name, type);
		Parameter param = new Parameter(PARAM_ID, PARAM_NAME, PARAM_TYPE, PARAM_VALUE_1);
		resource.addParameter(param);
		return resource;
	}

	public static void updateResource(Resource resource) {
		resource.setName(RES_NAME_MODIFIED);
		resource.getParameterBy(PARAM_ID).setValue(PARAM_VALUE_2);
	}

	public static Book createBook() {
		Book book = new Book(BOOK_ID, BOOK_TITLE, BOOK_AUTHOR, BOOK_PRESS_1, BOOK_PRICE);
		return book;
	}

	public static Book createBook(long id, String title, String author, String press, double price) {
		Book book = new Book(id, title, author, press, price);
		return book;
	}

	public static void updateBook(Book book) {
		book.setPress(BOOK_PRESS_2);
	}

	public static void assertBook(Book book) {
		Assert.assertNotNull(book);
		Assert.assertEquals(BOOK_ID, book.getId().longValue());
		Assert.assertEquals(BOOK_TITLE, book.getTitle());
		Assert.assertEquals(BOOK_AUTHOR, book.getAuthor());
		Assert.assertEquals(BOOK_PRESS_1, book.getPress());
		Assert.assertEquals(BOOK_PRICE, book.getPrice(), 0.0);
	}

	public static void assertBookUpdated(Book book) {
		Assert.assertNotNull(book);
		Assert.assertEquals(BOOK_ID, book.getId().longValue());
		Assert.assertEquals(BOOK_TITLE, book.getTitle());
		Assert.assertEquals(BOOK_AUTHOR, book.getAuthor());
		Assert.assertEquals(BOOK_PRESS_2, book.getPress());
		Assert.assertEquals(BOOK_PRICE, book.getPrice(), 0.0);
	}

	public static void assertResource(Resource resource) {
		Assert.assertNotNull(resource);
		Assert.assertEquals(RES_ID, resource.getId());
		Assert.assertEquals(RES_NAME, resource.getName());
		Assert.assertEquals(RES_TYPE, resource.getType());
		Parameter param = resource.getParameterBy(PARAM_ID);
		Assert.assertNotNull(param);
		Assert.assertEquals(PARAM_ID, param.getId());
		Assert.assertEquals(PARAM_NAME, param.getName());
		Assert.assertEquals(PARAM_TYPE, param.getType());
		Assert.assertEquals(PARAM_VALUE_1, param.getValue());
	}

	public static void assertResourceUpdated(Resource resource) {
		Assert.assertNotNull(resource);
		Assert.assertEquals(RES_ID, resource.getId());
		Assert.assertEquals(RES_NAME_MODIFIED, resource.getName());
		Assert.assertEquals(RES_TYPE, resource.getType());
		Parameter param = resource.getParameterBy(PARAM_ID);
		Assert.assertNotNull(param);
		Assert.assertEquals(PARAM_ID, param.getId());
		Assert.assertEquals(PARAM_NAME, param.getName());
		Assert.assertEquals(PARAM_TYPE, param.getType());
		Assert.assertEquals(PARAM_VALUE_2, param.getValue());
	}
}
