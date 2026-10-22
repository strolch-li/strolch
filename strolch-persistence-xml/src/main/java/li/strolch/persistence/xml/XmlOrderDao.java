/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.xmlpers.api.FileIo;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.util.FilenameUtility;

import java.io.File;
import java.util.*;
import java.util.function.Predicate;

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
		return queryAll(dateRange, Integer.MAX_VALUE, 0, true);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, String... types) throws StrolchPersistenceException {
		return queryAll(dateRange, Integer.MAX_VALUE, 0, true, types);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc)
			throws StrolchPersistenceException {
		Set<String> types = queryTypes();
		if (types.isEmpty())
			return Collections.emptyList();
		return queryAll(dateRange, limit, offset, asc, types.toArray(new String[0]));
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc, String... types)
			throws StrolchPersistenceException {
		if (types.length == 0)
			return queryAll(dateRange, limit, offset, asc);

		if (limit <= 0)
			return Collections.emptyList();

		record OrderRefAndDate(ObjectRef objectRef, long date) {}

		Predicate<File> dateRangePredicate = getDateRangePredicate(dateRange);
		List<OrderRefAndDate> orderRefs = new ArrayList<>();

		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			File queryPath = subTypeRef.getPath(this.tx.getManager().getPathBuilder());
			if (!queryPath.exists() || !queryPath.isDirectory())
				continue;

			File[] files = queryPath.listFiles();
			if (files == null)
				continue;

			for (File file : files) {
				if (file.isFile() && !file.getName().startsWith(FileIo.TMP_PREFIX) && dateRangePredicate.test(file)) {
					String id = FilenameUtility.getId(file.getName());
					ObjectRef childRef = subTypeRef.getChildIdRef(this.tx, id);
					orderRefs.add(new OrderRefAndDate(childRef, file.lastModified()));
				}
			}
		}

		if (orderRefs.isEmpty())
			return Collections.emptyList();

		Comparator<OrderRefAndDate> comparator;
		if (asc) {
			comparator = Comparator.comparingLong(OrderRefAndDate::date).thenComparing(o -> o.objectRef().getName());
		} else {
			comparator = Comparator.comparingLong(OrderRefAndDate::date).reversed().thenComparing(o -> o.objectRef().getName());
		}
		orderRefs.sort(comparator);

		List<ObjectRef> selectedRefs = orderRefs.stream()
				.skip(offset)
				.limit(limit)
				.map(OrderRefAndDate::objectRef)
				.toList();

		return this.tx.getObjectDao().queryAll(selectedRefs);
	}
}
