package li.strolch.runtime.agent.impl;

import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.model.query.StrolchQuery;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.agent.api.ElementMap;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @param <T>
 */
public abstract class TransactionalElementMap<T extends StrolchElement, U extends StrolchQuery<?>> implements
		ElementMap<T> {

	private PersistenceHandler persistenceHandler;
	private String realm;

	public TransactionalElementMap(String realm, PersistenceHandler persistenceHandler) {
		this.realm = realm;
		this.persistenceHandler = persistenceHandler;
	}

	protected String getRealm() {
		return this.realm;
	}

	protected PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}

	protected abstract StrolchDao<T> getDao(StrolchTransaction tx);

	@Override
	public boolean hasType(StrolchTransaction tx, String type) {
		return getDao(tx).queryTypes().contains(type);
	}

	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) {
		return getDao(tx).queryKeySet(type).contains(id);
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id) {
		return getDao(tx).queryBy(type, id);
	}

	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		return getDao(tx).queryAll();
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		return getDao(tx).queryAll(type);
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return getDao(tx).queryTypes();
	}

	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		return getDao(tx).queryKeySet();
	}

	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return getDao(tx).queryKeySet(type);
	}

	@Override
	public void add(StrolchTransaction tx, T element) {
		getDao(tx).save(element);
	}

	@Override
	public void update(StrolchTransaction tx, T element) {
		getDao(tx).update(element);
	}

	@Override
	public void remove(StrolchTransaction tx, T element) {
		getDao(tx).remove(element);
	}

	@Override
	public StrolchTransaction openTx() {
		return this.persistenceHandler.openTx();
	}

	@Override
	public StrolchTransaction openTx(String realm) {
		return this.persistenceHandler.openTx(realm);
	}
}
