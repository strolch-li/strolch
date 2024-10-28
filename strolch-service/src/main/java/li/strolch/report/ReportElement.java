/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.report;

import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public class ReportElement {

	private final List<String> columnKeys;
	private final Function<String, String> columnGetter;

	public ReportElement(List<String> columnKeys, Function<String, String> columnGetter) {
		this.columnGetter = columnGetter;
		this.columnKeys = columnKeys;
	}

	public List<String> getColumnKeys() {
		return this.columnKeys;
	}

	public String getColumn(String key) {
		return this.columnGetter.apply(key);
	}

	public Stream<SimpleEntry<String, String>> keyValueStream() {
		return this.columnKeys.stream().map(k -> new SimpleEntry<>(k, this.columnGetter.apply(k)));
	}

	public Stream<String> valueStream() {
		return this.columnKeys.stream().map(this.columnGetter);
	}
}
