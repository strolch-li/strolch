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

import java.util.List;

/**
 * Paging helper creating windows on result list of queries. The input is the original complete list, and with the help
 * of an offset and a limit, a page/window can be fetched.
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 *
 * @param <T>
 *            the type of element in the list
 */
public class Paging<T> {

	private int limit;
	private int offset;
	private int size;

	private List<T> input;
	private List<T> page;

	private int nextOffset;
	private int previousOffset;
	private int firstOffset;
	private int lastOffset;

	private Paging() {
		// empty constructor
	}

	/**
	 * @return the max number of elements to be returned in the page/window
	 */
	public int getLimit() {
		return this.limit;
	}

	/**
	 * @return the offset in the input list for the page/window
	 */
	public int getOffset() {
		return this.offset;
	}

	/**
	 * @return the next offset, i.e. offset + limit
	 */
	public int getNextOffset() {
		return this.nextOffset;
	}

	/**
	 * @return the previous offset, i.e. offset - limit
	 */
	public int getPreviousOffset() {
		return this.previousOffset;
	}

	/**
	 * @return 0, as the first offset is the first element in the input list
	 */
	public int getFirstOffset() {
		return this.firstOffset;
	}

	/**
	 * @return the last possible offset given the offset, limit and input size
	 */
	public int getLastOffset() {
		return this.lastOffset;
	}

	/**
	 * @return the size of the input list
	 */
	public int getSize() {
		return this.size;
	}

	/**
	 * @return the input
	 */
	public List<T> getInput() {
		return this.input;
	}

	/**
	 * @return the current page/window
	 */
	public List<T> getPage() {
		return this.page;
	}

	/**
	 * Creates a sub list of the input list with the given limit and offset
	 * 
	 * @param list
	 *            the list to paginate / create a window for
	 * @param offset
	 *            where to start the sub list
	 * @param limit
	 *            The number of items to return in each page/window
	 * @return a {@link Paging} instance from which the selected page (list) can be retrieved
	 * 
	 * @param <T>
	 *            the type of element in the list
	 */
	public static <T> Paging<T> asPage(List<T> list, int offset, int limit) {

		Paging<T> paging = new Paging<>();
		paging.firstOffset = 0;
		paging.limit = limit;
		paging.offset = offset;

		paging.size = list.size();
		paging.input = list;

		paging.firstOffset = 0;

		if (paging.limit <= 0 || paging.offset < 0) {
			paging.offset = 0;
			paging.limit = list.size();

			paging.page = list;

			paging.nextOffset = 0;
			paging.lastOffset = 0;
			paging.previousOffset = 0;

			return paging;
		}

		paging.page = list.subList(offset, Math.min(paging.size, offset + limit));

		if (limit == 1) {

			paging.nextOffset = Math.min(paging.size - 1, offset + 1);
			paging.previousOffset = Math.max(0, offset - 1);
			paging.lastOffset = paging.size - 1;

		} else {

			paging.nextOffset = Math.min(paging.size - 1, limit + offset);
			paging.previousOffset = Math.max(0, offset - limit);
			paging.lastOffset = offset + ((paging.size - offset) - ((paging.size - offset) % limit));
		}

		return paging;
	}
}
