/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.persistence.api;

import java.util.List;

import li.strolch.model.Order;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface OrderDao extends StrolchDao<Order> {

	/**
	 * Returns the number of elements in the underlying persistence layer, regardless of type
	 *
	 * @return the number of elements in the underlying persistence layer
	 */
	long querySize(DateRange dateRange);

	/**
	 * Returns the number of elements in the underlying persistence layer for the given type(s)
	 *
	 * @param dateRange
	 * 		the date range filter
	 * @param types
	 * 		the type(s) to query the size for
	 *
	 * @return the number of elements in the underlying persistence layer for the given type(s)
	 */
	long querySize(DateRange dateRange, String... types);

	/**
	 * Queries and returns all elements regardless of type
	 *
	 * @param dateRange
	 * 		the date range filter
	 *
	 * @return all elements regardless of type
	 *
	 * @throws StrolchPersistenceException
	 * 		if something goes wrong
	 */
	List<Order> queryAll(DateRange dateRange) throws StrolchPersistenceException;

	/**
	 * Queries and returns all elements regardless of type
	 *
	 * @param dateRange
	 * 		the date range filter
	 * @param limit
	 * 		the max amount, or @{@link Integer#MAX_VALUE} for all
	 * @param offset
	 * 		if max amount defined, then the offset to start from
	 *
	 * @return all elements regardless of type
	 *
	 * @throws StrolchPersistenceException
	 * 		if something goes wrong
	 */
	List<Order> queryAll(DateRange dateRange, long limit, long offset) throws StrolchPersistenceException;

	/**
	 * Queries and returns all elements of the given type
	 *
	 * @param dateRange
	 * 		the date range filter
	 * @param types
	 * 		the type(s) of element(s) to return
	 *
	 * @return all elements of the given type
	 *
	 * @throws StrolchPersistenceException
	 * 		if something goes wrong
	 */
	List<Order> queryAll(DateRange dateRange, String... types) throws StrolchPersistenceException;

	/**
	 * Queries and returns all elements of the given type
	 *
	 * @param dateRange
	 * 		the date range filter
	 * @param limit
	 * 		the max amount, or @{@link Integer#MAX_VALUE} for all
	 * @param offset
	 * 		if max amount defined, then the offset to start from
	 * @param types
	 * 		the type(s) of element(s) to return
	 *
	 * @return all elements of the given type
	 *
	 * @throws StrolchPersistenceException
	 * 		if something goes wrong
	 */
	List<Order> queryAll(DateRange dateRange, long limit, long offset, String... types)
			throws StrolchPersistenceException;

}
