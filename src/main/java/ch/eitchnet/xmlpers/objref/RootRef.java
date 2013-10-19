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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.realmName == null) ? 0 : this.realmName.hashCode());
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
		RootRef other = (RootRef) obj;
		if (this.realmName == null) {
			if (other.realmName != null)
				return false;
		} else if (!this.realmName.equals(other.realmName))
			return false;
		return true;
	}
}
