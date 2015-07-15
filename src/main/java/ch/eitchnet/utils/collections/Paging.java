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
package ch.eitchnet.utils.collections;

import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Paging<T> {

	private int resultsPerPage;
	private int indexOfPageToReturn;
	private int nrOfPages;

	private List<T> input;
	private List<T> page;

	private Paging(int resultsPerPage, int indexOfPageToReturn) {
		this.resultsPerPage = resultsPerPage;
		this.indexOfPageToReturn = indexOfPageToReturn;
	}

	public int getResultsPerPage() {
		return this.resultsPerPage;
	}

	public void setResultsPerPage(int resultsPerPage) {
		this.resultsPerPage = resultsPerPage;
	}

	public int getIndexOfPageToReturn() {
		return this.indexOfPageToReturn;
	}

	public void setIndexOfPageToReturn(int indexOfPageToReturn) {
		this.indexOfPageToReturn = indexOfPageToReturn;
	}

	public int getNrOfPages() {
		return this.nrOfPages;
	}

	public void setNrOfPages(int nrOfPages) {
		this.nrOfPages = nrOfPages;
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
	 * @param resultsPerPage
	 *            The number of items to return in each page
	 * @param page
	 *            the page to return - start index is 1
	 * 
	 * @return a {@link Paging} instance from which the selected page (list) can be retrieved
	 */
	public static <T> Paging<T> asPage(List<T> list, int resultsPerPage, int page) {

		Paging<T> paging = new Paging<T>(resultsPerPage, page);

		if (paging.resultsPerPage < 0 || paging.indexOfPageToReturn < 0) {
			paging.nrOfPages = -1;
			paging.input = list;
			paging.page = list;
			return paging;
		}

		int size = list.size();

		// calculate maximum number of pages
		paging.nrOfPages = size / paging.resultsPerPage;
		if (size % paging.resultsPerPage != 0)
			paging.nrOfPages++;

		// and from this validate requested page
		paging.indexOfPageToReturn = Math.min(paging.indexOfPageToReturn, paging.nrOfPages);

		// now we can calculate the start and end of the page
		int start = Math.max(0, paging.resultsPerPage * paging.indexOfPageToReturn - paging.resultsPerPage);
		int end = Math.min(size, paging.resultsPerPage * paging.indexOfPageToReturn);

		// and return the list
		paging.page = list.subList(start, end);

		return paging;
	}
}
