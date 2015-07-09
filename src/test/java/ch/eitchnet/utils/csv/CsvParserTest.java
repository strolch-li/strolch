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

import static org.junit.Assert.assertEquals;

import java.io.InputStream;
import java.util.Arrays;

import org.junit.Test;

import ch.eitchnet.utils.csv.CsvParser.CsvData;
import ch.eitchnet.utils.csv.CsvParser.CsvRow;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CsvParserTest {

	@Test
	public void shouldParseFile() {

		InputStream inputStream = CsvParserTest.class.getResourceAsStream("/test_data.csv");

		CsvParser csvParser = new CsvParser(inputStream);
		CsvData csvData = csvParser.parseData();

		assertEquals(3, csvData.getHeaders().size());
		assertEquals(Arrays.asList("title", "description", "nrOfPages"), csvData.getHeaders());
		assertEquals(3, csvData.getRows().size());

		CsvRow row = csvData.getRows().get(0);
		assertEquals("A", row.getColumnValue("title"));
		assertEquals("a", row.getColumnValue("description"));
		assertEquals("1", row.getColumnValue("nrOfPages"));

		row = csvData.getRows().get(2);
		assertEquals("C", row.getColumnValue("title"));
		assertEquals("c", row.getColumnValue("description"));
		assertEquals("3", row.getColumnValue("nrOfPages"));

		assertEquals(3, row.getValues().size());
	}
}
