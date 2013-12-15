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
package ch.eitchnet.xmlpers.objref;

import java.util.HashMap;
import java.util.Map;

import ch.eitchnet.utils.helper.StringHelper;

public class ObjectReferenceCache {

	private final String realmName;
	private final RootRef rootRef;
	private final Map<String, TypeRef> typeRefMap;
	private final Map<String, SubTypeRef> subTypeRefMap;
	private final Map<String, IdOfTypeRef> idOfTypeRefMap;
	private final Map<String, IdOfSubTypeRef> idOfSubTypeRefMap;

	public ObjectReferenceCache(String realmName) {
		if (StringHelper.isEmpty(realmName))
			throw new IllegalArgumentException("Realm name may not be empty!"); //$NON-NLS-1$

		this.realmName = realmName;
		this.rootRef = new RootRef(this.realmName);
		this.typeRefMap = new HashMap<>();
		this.subTypeRefMap = new HashMap<>();
		this.idOfTypeRefMap = new HashMap<>();
		this.idOfSubTypeRefMap = new HashMap<>();
	}

	public String getRealmName() {
		return this.realmName;
	}

	public synchronized RootRef getRootRef() {
		return this.rootRef;
	}

	public synchronized TypeRef getTypeRef(String type) {
		String key = RefNameCreator.createTypeName(this.realmName, type);
		TypeRef ref = this.typeRefMap.get(key);
		if (ref == null) {
			ref = new TypeRef(this.realmName, type);
			this.typeRefMap.put(key, ref);
		}

		return ref;
	}

	public synchronized SubTypeRef getSubTypeRef(String type, String subType) {
		String key = RefNameCreator.createSubTypeName(this.realmName, type, subType);
		SubTypeRef ref = this.subTypeRefMap.get(key);
		if (ref == null) {
			ref = new SubTypeRef(this.realmName, type, subType);
			this.subTypeRefMap.put(key, ref);
		}

		return ref;
	}

	public synchronized IdOfTypeRef getIdOfTypeRef(String type, String id) {
		String key = RefNameCreator.createIdOfTypeName(this.realmName, type, id);
		IdOfTypeRef idRef = this.idOfTypeRefMap.get(key);
		if (idRef == null) {
			idRef = new IdOfTypeRef(this.realmName, type, id);
			this.idOfTypeRefMap.put(key, idRef);
		}

		return idRef;
	}

	public synchronized IdOfSubTypeRef getIdOfSubTypeRef(String type, String subType, String id) {
		String key = RefNameCreator.createIdOfSubTypeName(this.realmName, type, subType, id);
		IdOfSubTypeRef idRef = this.idOfSubTypeRefMap.get(key);
		if (idRef == null) {
			idRef = new IdOfSubTypeRef(this.realmName, type, subType, id);
			this.idOfSubTypeRefMap.put(key, idRef);
		}

		return idRef;
	}
}
