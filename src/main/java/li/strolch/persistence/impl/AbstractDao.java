package li.strolch.persistence.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.SubTypeRef;
import ch.eitchnet.xmlpers.objref.TypeRef;

public abstract class AbstractDao<T extends StrolchElement> implements StrolchDao<T> {

	protected PersistenceTransaction tx;

	protected AbstractDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	protected abstract String getClassType();

	protected IdOfSubTypeRef getIdRef(String type, String id) {
		IdOfSubTypeRef idRef = this.tx.getObjectRefCache().getIdOfSubTypeRef(getClassType(), type, id);
		return idRef;
	}

	protected SubTypeRef getTypeRef(String type) {
		SubTypeRef typeRef = this.tx.getObjectRefCache().getSubTypeRef(getClassType(), type);
		return typeRef;
	}

	@Override
	public Set<String> queryKeySet() {
		Set<String> keys = new HashSet<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			keys.addAll(queryKeySet(type));
		}
		return keys;
	}

	@Override
	public Set<String> queryKeySet(String type) {
		SubTypeRef typeRef = this.tx.getObjectRefCache().getSubTypeRef(getClassType(), type);
		Set<String> keys = this.tx.getMetadataDao().queryKeySet(typeRef);
		return keys;
	}

	@Override
	public Set<String> queryTypes() {
		TypeRef typeRef = this.tx.getObjectRefCache().getTypeRef(getClassType());
		Set<String> types = this.tx.getMetadataDao().queryTypeSet(typeRef);
		return types;
	}

	@Override
	public T queryBy(String type, String id) {
		T t = this.tx.getObjectDao().queryById(getIdRef(type, id));
		return t;
	}

	@Override
	public List<T> queryAll() {
		List<T> objects = new ArrayList<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			List<T> objectsByType = this.tx.getObjectDao().queryAll(getTypeRef(type));
			objects.addAll(objectsByType);
		}

		return objects;
	}

	@Override
	public List<T> queryAll(String type) {
		List<T> objectsByType = this.tx.getObjectDao().queryAll(getTypeRef(type));
		return objectsByType;
	}

	@Override
	public void save(T object) {
		this.tx.getObjectDao().add(object);
	}

	@Override
	public void saveAll(List<T> objects) {
		this.tx.getObjectDao().addAll(objects);
	}

	@Override
	public void update(T object) {
		this.tx.getObjectDao().update(object);
	}

	@Override
	public void updateAll(List<T> objects) {
		this.tx.getObjectDao().updateAll(objects);
	}

	@Override
	public void remove(T object) {
		this.tx.getObjectDao().remove(object);
	}

	@Override
	public void removeAll(List<T> objects) {
		this.tx.getObjectDao().removeAll(objects);
	}

	@Override
	public void remove(String type, String id) {
		IdOfSubTypeRef objectRef = this.tx.getObjectRefCache().getIdOfSubTypeRef(getClassType(), type, id);
		this.tx.getObjectDao().removeById(objectRef);
	}
}
