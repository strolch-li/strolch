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
import java.util.List;

public class CsvData {
	private List<String> headers;
	private List<CsvRow> rows;

	public CsvData() {
		this.headers = new ArrayList<>();
		this.rows = new ArrayList<>();
	}

	public String getHeaderAtIndex(int column) {
		if (this.headers.size() < column + 1) {
			throw new IllegalArgumentException("No header exists at column index " + column);
		}
		return this.headers.get(column);
	}

	public void addHeader(String header) {
		this.headers.add(header);
	}

	public void addRow(CsvRow row) {
		this.rows.add(row);
	}

	public List<String> getHeaders() {
		return this.headers;
	}

	public List<CsvRow> getRows() {
		return this.rows;
	}
}