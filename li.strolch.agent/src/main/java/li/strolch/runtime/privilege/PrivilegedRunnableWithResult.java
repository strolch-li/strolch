package li.strolch.runtime.privilege;

import li.strolch.privilege.model.PrivilegeContext;

public interface PrivilegedRunnableWithResult<T> {

	public T run(PrivilegeContext ctx) throws Exception;
}