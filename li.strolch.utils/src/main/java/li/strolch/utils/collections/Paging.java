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
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 *
 * @param <T>
 *            the type of element in the list
 */
public class Paging<T> {

	private int pageSize;
	private int pageToReturn;
	private int nrOfPages;
	private int nrOfElements;

	private List<T> input;
	private List<T> page;

	private Paging(int pageSize, int indexOfPageToReturn) {
		this.pageSize = pageSize;
		this.pageToReturn = indexOfPageToReturn;
	}

	public int getPageSize() {
		return this.pageSize;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public int getPageToReturn() {
		return this.pageToReturn;
	}

	public void setPageToReturn(int pageToReturn) {
		this.pageToReturn = pageToReturn;
	}

	public int getNrOfPages() {
		return this.nrOfPages;
	}

	public void setNrOfPages(int nrOfPages) {
		this.nrOfPages = nrOfPages;
	}

	public int getNrOfElements() {
		return this.nrOfElements;
	}

	public void setNrOfElements(int nrOfElements) {
		this.nrOfElements = nrOfElements;
	}

	public List<T> getInput() {
		return this.input;
	}

	public List<T> getPage() {
		return this.page;
	}

	/**
	 * Creates a sub list of the given list by creating defining start and end from the requested page of the form
	 * 
	 * @param list
	 *            the list to paginate
	 * @param pageSize
	 *            The number of items to return in each page
	 * @param page
	 *            the page to return - start index is 1
	 * 
	 * @return a {@link Paging} instance from which the selected page (list) can be retrieved
	 * 
	 * @param <T>
	 *            the type of element in the list
	 */
	public static <T> Paging<T> asPage(List<T> list, int pageSize, int page) {

		Paging<T> paging = new Paging<>(pageSize, page);
		paging.nrOfElements = list.size();

		if (paging.pageSize <= 0 || paging.pageToReturn <= 0) {
			paging.nrOfPages = 0;
			paging.pageSize = list.size();
			paging.pageToReturn = 0;
			paging.input = list;
			paging.page = list;
			return paging;
		}

		int size = list.size();

		// calculate maximum number of pages
		paging.nrOfPages = size / paging.pageSize;
		if (size % paging.pageSize != 0)
			paging.nrOfPages++;

		// and from this validate requested page
		paging.pageToReturn = Math.min(paging.pageToReturn, paging.nrOfPages);

		// now we can calculate the start and end of the page
		int start = Math.max(0, paging.pageSize * paging.pageToReturn - paging.pageSize);
		int end = Math.min(size, paging.pageSize * paging.pageToReturn);

		// and return the list
		paging.page = list.subList(start, end);

		// fix page size
		if (paging.page.size() < paging.pageSize)
			paging.pageSize = paging.page.size();

		return paging;
	}
}
