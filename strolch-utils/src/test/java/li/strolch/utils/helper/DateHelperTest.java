/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils.helper;

import org.junit.Before;
import org.junit.Test;

import java.util.Locale;
import java.util.ResourceBundle;

import static org.junit.Assert.assertEquals;

public class DateHelperTest {

	private Locale locale;

	@Before
	public void before() {
		this.locale = new Locale("de", "CH");
	}

	@Test
	public void test1() {
		String isoDate = "2018-01-21T04:01:00.000+02:00";
		String result = DateHelper.formatDate(this.locale, isoDate, true);
		assertEquals("21.01.2018 04:01:00", result);
	}

	@Test
	public void test2() {
		String isoDate = "2018-01-21T00:00:00.000+02:00";
		String result = DateHelper.formatDate(this.locale, isoDate, false);
		assertEquals("21.01.2018", result);
	}

	@Test
	public void test3() {
		String isoDate = "2018-01-21T04:01:00.000+02:00";
		String result = DateHelper.formatDate(this.locale, isoDate, false);
		assertEquals("21.01.2018", result);
	}

	@Test
	public void test4() {
		String isoPeriod = "P4W";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), null, isoPeriod);
		assertEquals("4 weeks", result);
	}

	@Test
	public void test5() {
		String isoPeriod = "P1W";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), "after_every", isoPeriod);
		assertEquals("after every 1 week", result);
	}

	@Test
	public void test6() {
		String isoPeriod = "P2M";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), "after_every", isoPeriod);
		assertEquals("after every 2 months", result);
	}

	@Test
	public void test7() {
		String isoPeriod = "P1M";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), null, isoPeriod);
		assertEquals("1 month", result);
	}

	@Test
	public void test8() {
		String isoPeriod = "P1D";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), "after_every", isoPeriod);
		assertEquals("after every 1 day", result);
	}

	@Test
	public void test9() {
		String isoPeriod = "P2D";
		String result = DateHelper.formatPeriod(ResourceBundle.getBundle("UtilsTest"), "after_every", isoPeriod);
		assertEquals("after every 2 days", result);
	}
}
