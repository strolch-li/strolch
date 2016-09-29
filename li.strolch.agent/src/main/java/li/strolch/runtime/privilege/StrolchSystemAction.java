package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.model.PrivilegeContext;

/**
 * {@link SystemAction} to run {@link PrivilegedRunnable} as a system user
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchSystemAction extends SystemAction {

	private PrivilegedRunnable runnable;

	public StrolchSystemAction(PrivilegedRunnable runnable) {
		this.runnable = runnable;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.runnable.run(privilegeContext);
	}
}
