package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.handler.SystemActionWithResult;
import li.strolch.privilege.model.PrivilegeContext;

/**
 * {@link SystemAction} to run {@link PrivilegedRunnableWithResult} as a system user
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 */
public class StrolchSystemActionWithResult<T> extends SystemActionWithResult<T> {

	private PrivilegedRunnableWithResult<T> runnable;

	public StrolchSystemActionWithResult(PrivilegedRunnableWithResult<T> runnable) {
		this.runnable = runnable;
	}

	@Override
	public T execute(PrivilegeContext privilegeContext) {
		return this.runnable.run(privilegeContext);
	}
}
