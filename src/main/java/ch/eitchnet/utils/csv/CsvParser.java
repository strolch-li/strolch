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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.xml.XmlKeyValue;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CsvParser {

	private InputStream inputStream;

	/**
	 * @param inputStream
	 */
	public CsvParser(InputStream inputStream) {
		DBC.PRE.assertNotNull("InputStream may not be null!", inputStream);
		this.inputStream = inputStream;
	}

	public CsvData parseData() {

		CsvData data = new CsvData();

		int lineNr = 0;
		boolean headerRead = false;

		try {
			try (BufferedReader reader = new BufferedReader(new InputStreamReader(this.inputStream))) {
				String line;
				while ((line = reader.readLine()) != null) {

					line = line.trim();
					if (line.isEmpty())
						continue;
					if (line.charAt(0) == '#')
						continue;

					try (Scanner scanner = new Scanner(line)) {
						scanner.useDelimiter(";");
						if (headerRead) {
							int column = 0;
							CsvRow row = new CsvRow(lineNr);
							while (scanner.hasNext()) {
								row.addColumnValue(data.getHeaderAtIndex(column), scanner.next());
								column++;
							}
							data.addRow(row);
						} else {
							while (scanner.hasNext()) {
								data.addHeader(scanner.next().trim());
							}
							headerRead = true;
						}
					}

					lineNr++;
				}
			}
		} catch (IOException e) {
			throw new RuntimeException("Failed to read csv data at line " + lineNr + " due to " + e.getMessage(), e);
		}

		return data;
	}

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

	public class CsvRow {
		private int index;
		private List<XmlKeyValue> values;

		public CsvRow(int index) {
			this.index = index;
			this.values = new ArrayList<>();
		}

		public int getIndex() {
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
}
