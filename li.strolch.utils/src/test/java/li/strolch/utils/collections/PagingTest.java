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

import li.strolch.utils.collections.Paging;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PagingTest {

	@Test
	public void shouldReturnAll() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, -1, -1).getPage();
		assertEquals(list, page);
	}

	@Test
	public void shouldReturnFirstPage1() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 1, 1).getPage();
		assertEquals(Arrays.asList("a"), page);
	}

	@Test
	public void shouldReturnFirstPage2() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 2, 1).getPage();
		assertEquals(Arrays.asList("a", "b"), page);
	}

	@Test
	public void shouldReturnSecondPage1() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 1, 2).getPage();
		assertEquals(Arrays.asList("b"), page);
	}

	@Test
	public void shouldReturnSecondPage2() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 2, 2).getPage();
		assertEquals(Arrays.asList("c", "d"), page);
	}

	@Test
	public void shouldReturnLastPage1() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 1, 6).getPage();
		assertEquals(Arrays.asList("f"), page);
	}

	@Test
	public void shouldReturnLastPage2() {
		List<String> list = Arrays.asList("a", "b", "c", "d", "e", "f");
		List<String> page = Paging.asPage(list, 2, 3).getPage();
		assertEquals(Arrays.asList("e", "f"), page);
	}
}
