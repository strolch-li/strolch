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
package li.strolch.persistence.postgresql;

import java.util.ArrayList;
import java.util.List;

public class ModificationResult {

	private final String key;
	private final List<?> created;
	private final List<?> updated;
	private final List<?> deleted;

	public ModificationResult(String key) {
		this.key = key;
		this.created = new ArrayList<>();
		this.updated = new ArrayList<>();
		this.deleted = new ArrayList<>();
	}

	public ModificationResult(String key, List<?> created, List<?> updated, List<?> deleted) {
		this.key = key;
		this.created = created;
		this.updated = updated;
		this.deleted = deleted;
	}

	public String getKey() {
		return this.key;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getCreated() {
		return (List<T>) this.created;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getUpdated() {
		return (List<T>) this.updated;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getDeleted() {
		return (List<T>) this.deleted;
	}
}
