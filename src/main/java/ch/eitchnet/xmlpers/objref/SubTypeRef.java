package ch.eitchnet.xmlpers.objref;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public class SubTypeRef extends ObjectRef {

	private final String type;
	private final String subType;

	public SubTypeRef(String realmName, String type, String subType) {
		super(realmName, RefNameCreator.createSubTypeName(realmName, type, subType));
		this.type = type;
		this.subType = subType;
	}

	@Override
	public String getType() {
		return this.type;
	}

	public String getSubType() {
		return this.subType;
	}

	@Override
	public boolean isRoot() {
		return false;
	}

	@Override
	public boolean isLeaf() {
		return false;
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		return tx.getRealm().getObjectRefCache().getTypeRef(this.type);
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		return tx.getRealm().getObjectRefCache().getIdOfSubTypeRef(this.type, this.subType, id);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		String msg = MessageFormat.format("A SubType has no child type: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getSubTypePath(this.type, this.subType);
	}
	
	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat.format("{0} is not a leaf and can thus not have a Persistence Context", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}
}
