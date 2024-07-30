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
package li.strolch.xmlpers.objref;

import java.util.HashMap;
import java.util.Map;

public class ObjectReferenceCache {

	private final RootRef rootRef;
	private final Map<String, TypeRef> typeRefMap;
	private final Map<String, SubTypeRef> subTypeRefMap;
	private final Map<String, IdOfTypeRef> idOfTypeRefMap;
	private final Map<String, IdOfSubTypeRef> idOfSubTypeRefMap;

	public ObjectReferenceCache() {
		this.rootRef = new RootRef();
		this.typeRefMap = new HashMap<>();
		this.subTypeRefMap = new HashMap<>();
		this.idOfTypeRefMap = new HashMap<>();
		this.idOfSubTypeRefMap = new HashMap<>();
	}

	public synchronized TypeRef getTypeRef(String type) {
		String key = RefNameCreator.createTypeName(type);
		TypeRef ref = this.typeRefMap.get(key);
		if (ref == null) {
			ref = new TypeRef(this.rootRef, type);
			this.typeRefMap.put(key, ref);
		}

		return ref;
	}

	public synchronized SubTypeRef getSubTypeRef(String type, String subType) {
		String key = RefNameCreator.createSubTypeName(type, subType);
		SubTypeRef ref = this.subTypeRefMap.get(key);
		if (ref == null) {
			ref = new SubTypeRef(getTypeRef(type), subType);
			this.subTypeRefMap.put(key, ref);
		}

		return ref;
	}

	public synchronized IdOfTypeRef getIdOfTypeRef(String type, String id) {
		String key = RefNameCreator.createIdOfTypeName(type, id);
		IdOfTypeRef idRef = this.idOfTypeRefMap.get(key);
		if (idRef == null) {
			idRef = new IdOfTypeRef(getTypeRef(type), id);
			this.idOfTypeRefMap.put(key, idRef);
		}

		return idRef;
	}

	public synchronized IdOfSubTypeRef getIdOfSubTypeRef(String type, String subType, String id) {
		String key = RefNameCreator.createIdOfSubTypeName(type, subType, id);
		IdOfSubTypeRef idRef = this.idOfSubTypeRefMap.get(key);
		if (idRef == null) {
			idRef = new IdOfSubTypeRef(getSubTypeRef(type, subType), id);
			this.idOfSubTypeRefMap.put(key, idRef);
		}

		return idRef;
	}
}
