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
package li.strolch.xmlpers.test.impl;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceContextFactory;
import li.strolch.xmlpers.objref.IdOfTypeRef;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.ObjectReferenceCache;
import li.strolch.xmlpers.test.model.Book;

public class BookContextFactory implements PersistenceContextFactory<Book> {

	@Override
	public PersistenceContext<Book> createCtx(ObjectRef objectRef) {
		PersistenceContext<Book> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new BookParserFactory());
		return ctx;
	}

	@Override
	public PersistenceContext<Book> createCtx(ObjectReferenceCache objectRefCache, Book t) {
		IdOfTypeRef objectRef = objectRefCache.getIdOfTypeRef(TestConstants.TYPE_BOOK, t.getId().toString());
		PersistenceContext<Book> ctx = createCtx(objectRef);
		ctx.setObject(t);
		return ctx;
	}
}
