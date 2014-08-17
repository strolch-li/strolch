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
package ch.eitchnet.xmlpers.test.impl;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;
import ch.eitchnet.xmlpers.test.model.MyModel;

public class MyModelContextFactory implements PersistenceContextFactory<MyModel> {

	@Override
	public PersistenceContext<MyModel> createCtx(ObjectReferenceCache objectRefCache, MyModel t) {
		IdOfSubTypeRef objectRef = objectRefCache.getIdOfSubTypeRef(TestConstants.TYPE_RES, t.getType(), t.getId());
		return createCtx(objectRef);
	}

	@Override
	public PersistenceContext<MyModel> createCtx(ObjectRef objectRef) {
		PersistenceContext<MyModel> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new MyModelParserFactory());
		return ctx;
	}

}