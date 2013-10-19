package ch.eitchnet.xmlpers.objref;

import java.io.File;
import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public class RootRef extends ObjectRef {

	public RootRef(String realmName) {
		super(realmName, RefNameCreator.createRootName(realmName));
	}

	@Override
	public boolean isRoot() {
		return true;
	}

	@Override
	public boolean isLeaf() {
		return false;
	}

	@Override
	public String getType() {
		String msg = MessageFormat.format("RootRef has no type: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		String msg = MessageFormat.format("RootRef has no parent: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		String msg = MessageFormat.format("RootRef has no child id: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		return tx.getRealm().getObjectRefCache().getTypeRef(type);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getRootPath();
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat.format("{0} is not a leaf and can thus not have a Persistence Context", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}
}
