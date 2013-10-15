package ch.eitchnet.xmlpers.objref;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.api.PersistenceContextFactoryDelegator;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public class IdOfSubTypeRef extends ObjectRef {

	private final String type;
	private final String subType;
	private final String id;

	public IdOfSubTypeRef(String realmName, String type, String subType, String id) {
		super(realmName, RefNameCreator.createIdOfSubTypeName(realmName, type, subType, id));
		this.type = type;
		this.subType = subType;
		this.id = id;
	}

	public String getType() {
		return this.type;
	}

	public String getSubType() {
		return this.subType;
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
		return tx.getRealm().getObjectRefCache().getSubTypeRef(this.type, this.subType);
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
		return pathBuilder.getIdOfSubTypePath(this.type, this.subType, this.id);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		PersistenceContextFactoryDelegator ctxFactoryDelegator = tx.getRealm().getCtxFactoryDelegator();
		PersistenceContextFactory<T> persistenceContextFactory = ctxFactoryDelegator
				.<T> getCtxFactory(this.type);
		return persistenceContextFactory.createCtx(this);
	}
}
