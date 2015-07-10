/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package ch.eitchnet.utils.csv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import ch.eitchnet.utils.xml.XmlKeyValue;

public class CsvRow {
	private long index;
	private List<XmlKeyValue> values;

	public CsvRow(long index) {
		this.index = index;
		this.values = new ArrayList<>();
	}

	public long getIndex() {
		return this.index;
	}

	public void addColumnValue(String header, String value) {
		this.values.add(new XmlKeyValue(header, value));
	}

	public String getColumnValue(String header) {
		for (Iterator<XmlKeyValue> iter = this.values.iterator(); iter.hasNext();) {
			XmlKeyValue next = iter.next();
			if (next.getKey().equals(header))
				return next.getValue();
		}
		throw new IllegalArgumentException("No value exists for header" + header);
	}

	public List<XmlKeyValue> getValues() {
		return this.values;
	}
}