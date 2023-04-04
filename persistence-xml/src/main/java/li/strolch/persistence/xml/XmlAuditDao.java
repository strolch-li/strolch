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
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import li.strolch.model.Tags;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.IdOfSubTypeRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.objref.TypeRef;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlAuditDao implements AuditDao {

	private final PersistenceTransaction tx;

	public XmlAuditDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	protected String getClassType() {
		return Tags.AUDIT;
	}

	protected IdOfSubTypeRef getIdRef(String type, Long id) {
		return this.tx.getManager().getObjectRefCache().getIdOfSubTypeRef(getClassType(), type, id.toString());
	}

	protected SubTypeRef getTypeRef(String type) {
		return this.tx.getManager().getObjectRefCache().getSubTypeRef(getClassType(), type);
	}

	@Override
	public boolean hasElement(String type, Long id) {
		IdOfSubTypeRef ref = getIdRef(type, id);
		return this.tx.getObjectDao().hasElement(ref);
	}

	@Override
	public long querySize(DateRange dateRange) {
		long size = 0;
		Set<String> types = queryTypes();
		for (String type : types) {
			size += querySize(type, dateRange);
		}
		return size;
	}

	@Override
	public long querySize(String type, DateRange dateRange) {
		Predicate<File> predicate = file -> dateRange.contains(new Date(file.lastModified()));
		return this.tx.getMetadataDao().querySize(getTypeRef(type), predicate);
	}

	@Override
	public Set<String> queryTypes() {
		TypeRef typeRef = this.tx.getManager().getObjectRefCache().getTypeRef(getClassType());
		return this.tx.getMetadataDao().queryTypeSet(typeRef);
	}

	@Override
	public Audit queryBy(String type, Long id) {
		return this.tx.getObjectDao().queryById(getIdRef(type, id));
	}

	@Override
	public List<Audit> queryAll(String type, DateRange dateRange) {
		Predicate<File> predicate = file -> dateRange.contains(new Date(file.lastModified()));
		return this.tx.getObjectDao().queryAll(getTypeRef(type), predicate);
	}

	@Override
	public void save(Audit audit) {
		PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
		this.tx.getFileDao().performCreate(ctx);
	}

	@Override
	public void saveAll(List<Audit> audits) {
		for (Audit audit : audits) {
			PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
			this.tx.getFileDao().performCreate(ctx);
		}
	}

	@Override
	public void update(Audit audit) {
		PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
		this.tx.getFileDao().performUpdate(ctx);
	}

	@Override
	public void updateAll(List<Audit> audits) {
		for (Audit audit : audits) {
			PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
			this.tx.getFileDao().performUpdate(ctx);
		}
	}

	@Override
	public void remove(Audit audit) {
		PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
		this.tx.getFileDao().performDelete(ctx);
	}

	@Override
	public void removeAll(List<Audit> audits) {
		for (Audit audit : audits) {
			PersistenceContext<Audit> ctx = this.tx.getObjectDao().createCtx(audit, -1L);
			this.tx.getFileDao().performDelete(ctx);
		}
	}

	@Override
	public long removeAll(String type, DateRange dateRange) {
		Predicate<File> predicate = file -> dateRange.contains(new Date(file.lastModified()));
		return this.tx.getObjectDao().removeAllBy(getTypeRef(type), predicate);
	}
}
