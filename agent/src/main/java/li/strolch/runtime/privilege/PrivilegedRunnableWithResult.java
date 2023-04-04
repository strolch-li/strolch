package li.strolch.runtime.privilege;

import li.strolch.privilege.model.PrivilegeContext;

public interface PrivilegedRunnableWithResult<T> {

	T run(PrivilegeContext ctx);
}