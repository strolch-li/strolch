package ch.eitchnet.xmlpers.objref;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public class TypeRef extends ObjectRef {

	private final String type;

	public TypeRef(String realmName, String type) {
		super(realmName, RefNameCreator.createTypeName(realmName, type));
		this.type = type;
	}

	@Override
	public String getType() {
		return this.type;
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
		return tx.getRealm().getObjectRefCache().getRootRef();
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		return tx.getRealm().getObjectRefCache().getIdOfTypeRef(this.type, id);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		return tx.getRealm().getObjectRefCache().getSubTypeRef(this.type, type);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getTypePath(this.type);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat.format("{0} is not a leaf and can thus not have a Persistence Context", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}
}
