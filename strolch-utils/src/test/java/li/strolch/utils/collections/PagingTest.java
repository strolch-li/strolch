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
package li.strolch.utils.collections;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class PagingTest {

	private List<String> getInputList() {
		return Arrays.asList("a", "b", "c", "d", "e", "f", "g");
	}

	@Test
	public void shouldReturnAll() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, -1, -1).getPage();
		assertEquals(list, page);
		page = Paging.asPage(list, 0, 0).getPage();
		assertEquals(list, page);
	}

	@Test
	public void shouldReturnFirstPage1() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 0, 1).getPage();
		assertEquals(List.of("a"), page);
	}

	@Test
	public void shouldReturnFirstPage2() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 0, 2).getPage();
		assertEquals(Arrays.asList("a", "b"), page);
	}

	@Test
	public void shouldReturnSecondPage1() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 1, 1).getPage();
		assertEquals(List.of("b"), page);
	}

	@Test
	public void shouldReturnSecondPage2() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 2, 2).getPage();
		assertEquals(Arrays.asList("c", "d"), page);
	}

	@Test
	public void shouldReturnLastPage1() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 5, 1).getPage();
		assertEquals(List.of("f"), page);
	}

	@Test
	public void shouldReturnLastPage2() {
		List<String> list = getInputList();
		List<String> page = Paging.asPage(list, 4, 2).getPage();
		assertEquals(Arrays.asList("e", "f"), page);
	}

	@Test
	public void shouldReturnLastPage3() {
		List<String> list = getInputList();

		Paging<String> paging = Paging.asPage(list, 0, 2);
		List<String> page = paging.getPage();
		assertEquals(Arrays.asList("a", "b"), page);
		assertEquals(2, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());

		paging = Paging.asPage(list, paging.getLastOffset(), 2);
		page = paging.getPage();
		assertEquals(List.of("g"), page);
		assertEquals(6, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());
	}

	@Test
	public void shouldReturnLastPage4() {
		List<String> list = getInputList();

		Paging<String> paging = Paging.asPage(list, 0, 1);
		List<String> page = paging.getPage();
		assertEquals(List.of("a"), page);

		paging = Paging.asPage(list, paging.getLastOffset(), 1);
		page = paging.getPage();
		assertEquals(List.of("g"), page);
		assertEquals(6, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());
	}

	@Test
	public void shouldReturnLastPage5() {
		List<String> list = getInputList();

		Paging<String> paging = Paging.asPage(list, 0, 3);
		List<String> page = paging.getPage();
		assertEquals(Arrays.asList("a", "b", "c"), page);
		assertEquals(0, paging.getOffset());
		assertEquals(0, paging.getPreviousOffset());
		assertEquals(3, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("d", "e", "f"), page);
		assertEquals(3, paging.getOffset());
		assertEquals(0, paging.getPreviousOffset());
		assertEquals(6, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());

		paging = Paging.asPage(list, paging.getLastOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(List.of("g"), page);
		assertEquals(6, paging.getOffset());
		assertEquals(3, paging.getPreviousOffset());
		assertEquals(6, paging.getNextOffset());
		assertEquals(6, paging.getLastOffset());
	}

	@Test
	public void shouldReturnLastPage6() {
		List<String> list = getInputList();

		Paging<String> paging = Paging.asPage(list, 1, 1);
		List<String> page = paging.getPage();
		assertEquals(List.of("b"), page);

		paging = Paging.asPage(list, paging.getPreviousOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(List.of("a"), page);

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(List.of("b"), page);

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(List.of("c"), page);

		paging = Paging.asPage(list, paging.getLastOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(List.of("g"), page);
	}

	@Test
	public void shouldReturnLastPage7() {
		List<String> list = getInputList();

		Paging<String> paging = Paging.asPage(list, 2, 2);
		List<String> page = paging.getPage();
		assertEquals(Arrays.asList("c", "d"), page);

		paging = Paging.asPage(list, paging.getPreviousOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("a", "b"), page);

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("c", "d"), page);

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("e", "f"), page);
	}

	@Test
	public void shouldReturnLastPage8() {
		List<String> list = Arrays.asList("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11");

		Paging<String> paging = Paging.asPage(list, 0, 5);
		List<String> page = paging.getPage();
		assertEquals(Arrays.asList("0", "1", "2", "3", "4"), page);
		assertEquals(0, paging.getOffset());
		assertEquals(0, paging.getPreviousOffset());
		assertEquals(5, paging.getNextOffset());
		assertEquals(10, paging.getLastOffset());

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("5", "6", "7", "8", "9"), page);
		assertEquals(5, paging.getOffset());
		assertEquals(0, paging.getPreviousOffset());
		assertEquals(10, paging.getNextOffset());
		assertEquals(10, paging.getLastOffset());

		paging = Paging.asPage(list, paging.getNextOffset(), paging.getLimit());
		page = paging.getPage();
		assertEquals(Arrays.asList("10", "11"), page);
		assertEquals(10, paging.getOffset());
		assertEquals(5, paging.getPreviousOffset());
		assertEquals(10, paging.getNextOffset());
		assertEquals(10, paging.getLastOffset());
	}
}
