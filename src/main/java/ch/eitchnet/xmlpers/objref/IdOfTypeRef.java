package ch.eitchnet.xmlpers.objref;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.api.PersistenceContextFactoryDelegator;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public class IdOfTypeRef extends ObjectRef {

	private final String type;
	private final String id;

	public IdOfTypeRef(String realmName, String type, String id) {
		super(realmName, RefNameCreator.createIdOfTypeName(realmName, type, id));
		this.type = type;
		this.id = id;
	}

	@Override
	public String getType() {
		return this.type;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	@Override
	public boolean isRoot() {
		return false;
	}

	@Override
	public boolean isLeaf() {
		return true;
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		return tx.getRealm().getObjectRefCache().getTypeRef(this.type);
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getIdOfTypePath(this.type, this.id);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		PersistenceContextFactoryDelegator ctxFactoryDelegator = tx.getRealm().getCtxFactoryDelegator();
		PersistenceContextFactory<T> persistenceContextFactory = ctxFactoryDelegator.<T> getCtxFactory(this.type);
		return persistenceContextFactory.createCtx(this);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		result = prime * result + ((this.realmName == null) ? 0 : this.realmName.hashCode());
		result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IdOfTypeRef other = (IdOfTypeRef) obj;
		if (this.realmName == null) {
			if (other.realmName != null)
				return false;
		} else if (!this.realmName.equals(other.realmName))
			return false;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		if (this.type == null) {
			if (other.type != null)
				return false;
		} else if (!this.type.equals(other.type))
			return false;
		return true;
	}
}
