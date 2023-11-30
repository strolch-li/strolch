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
package li.strolch.persistence.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.xmlpers.objref.SubTypeRef;

public class XmlOrderDao extends AbstractDao<Order> implements OrderDao {

	protected XmlOrderDao(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassType() {
		return Tags.ORDER;
	}

	@Override
	public void save(Order object) {
		this.tx.getObjectDao().add(object, object.getDate().getTime());
	}

	@Override
	public void saveAll(List<Order> objects) {
		for (Order object : objects) {
			save(object);
		}
	}

	@Override
	public void update(Order object) {
		this.tx.getObjectDao().update(object, object.getDate().getTime());
	}

	@Override
	public void updateAll(List<Order> objects) {
		for (Order object : objects) {
			update(object);
		}
	}

	private Predicate<File> getDateRangePredicate(DateRange dateRange) {
		return file -> dateRange.contains(new Date(file.lastModified()));
	}

	@Override
	public long querySize(DateRange dateRange) {
		long size = 0;
		Set<String> types = queryTypes();
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			size += this.tx.getMetadataDao().querySize(subTypeRef, getDateRangePredicate(dateRange));
		}
		return size;
	}

	@Override
	public long querySize(DateRange dateRange, String... types) {

		if (types.length == 0)
			return querySize();

		if (types.length == 1) {
			SubTypeRef subTypeRef = getTypeRef(types[0]);
			return this.tx.getMetadataDao().querySize(subTypeRef, getDateRangePredicate(dateRange));
		}

		long size = 0;
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			size += this.tx.getMetadataDao().querySize(subTypeRef, getDateRangePredicate(dateRange));
		}

		return size;
	}

	@Override
	public List<Order> queryAll(DateRange dateRange) throws StrolchPersistenceException {
		List<Order> objects = new ArrayList<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			List<Order> objectsByType = this.tx.getObjectDao()
					.queryAll(getTypeRef(type), getDateRangePredicate(dateRange));
			objects.addAll(objectsByType);
		}

		return objects;
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc)
			throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Paging not supported! Check first with supportsPaging()");
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, String... types) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Paging not supported! Check first with supportsPaging()");
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc, String... types)
			throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Paging not supported! Check first with supportsPaging()");
	}
}
