package li.strolch.runtime.privilege;

import li.strolch.privilege.model.PrivilegeContext;

public interface PrivilegedRunnable {

	void run(PrivilegeContext ctx) throws Exception;
}