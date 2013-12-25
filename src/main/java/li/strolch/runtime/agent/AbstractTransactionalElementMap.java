package li.strolch.runtime.agent;

import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;

public abstract class AbstractTransactionalElementMap<T extends StrolchElement> implements ElementMap<T> {

	private PersistenceHandler persistenceHandler;
	private String realm;

	public AbstractTransactionalElementMap(String realm, PersistenceHandler persistenceHandler) {
		this.realm = realm;
		this.persistenceHandler = persistenceHandler;
	}

	protected abstract StrolchDao<T> getDao(StrolchTransaction tx);

	@Override
	public boolean hasType(String type) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryTypes().contains(type);
		}
	}

	@Override
	public boolean hasElement(String type, String id) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryKeySet(type).contains(id);
		}
	}

	@Override
	public T getBy(String type, String id) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryBy(type, id);
		}
	}

	@Override
	public List<T> getAllElements() {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryAll();
		}
	}

	@Override
	public List<T> getElementsBy(String type) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryAll(type);
		}
	}

	@Override
	public Set<String> getTypes() {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryTypes();
		}
	}

	@Override
	public Set<String> getAllKeys() {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryKeySet();
		}
	}

	@Override
	public Set<String> getKeysBy(String type) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			return getDao(tx).queryKeySet(type);
		}
	}

	@Override
	public void add(T element) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			getDao(tx).save(element);
		}
	}

	@Override
	public void update(T element) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			getDao(tx).update(element);
		}
	}

	@Override
	public void remove(T element) {
		try (StrolchTransaction tx = this.persistenceHandler.openTx(this.realm)) {
			getDao(tx).remove(element);
		}
	}
}
