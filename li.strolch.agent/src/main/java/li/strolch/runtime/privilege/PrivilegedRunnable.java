package li.strolch.runtime.privilege;

import li.strolch.privilege.model.PrivilegeContext;

public interface PrivilegedRunnable {

	public void run(PrivilegeContext ctx) throws Exception;
}