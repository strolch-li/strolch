package ch.eitchnet.xmlpers.objref;

import java.io.File;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public abstract class ObjectRef extends LockableObject {

	protected final String realmName;
	protected final String name;

	protected ObjectRef(String realmName, String name) {
		this.realmName = realmName;
		this.name = name;
	}

	public String getRealmName() {
		return this.realmName;
	}

	public String getName() {
		return this.name;
	}

	public abstract boolean isRoot();

	public abstract boolean isLeaf();

	public abstract String getType();

	public abstract ObjectRef getParent(PersistenceTransaction tx);

	public abstract ObjectRef getChildIdRef(PersistenceTransaction tx, String id);

	public abstract ObjectRef getChildTypeRef(PersistenceTransaction tx, String type);

	public abstract File getPath(PathBuilder pathBuilder);

	public abstract <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx);

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public abstract boolean equals(Object obj);

	@Override
	public abstract int hashCode();
}
