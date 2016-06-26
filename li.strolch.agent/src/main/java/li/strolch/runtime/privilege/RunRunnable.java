package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemUserAction;
import li.strolch.privilege.model.PrivilegeContext;

/**
 * {@link SystemUserAction} to run {@link Runnable} as a system user
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 * @param <U>
 */
public class RunRunnable<T> extends SystemUserAction {

	private Runnable<T> runnable;
	private T result;

	public RunRunnable(Runnable<T> runnable) {
		this.runnable = runnable;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.result = runnable.run(privilegeContext);
	}

	public T getResult() {
		return result;
	}

	public interface Runnable<T> {

		public T run(PrivilegeContext ctx);
	}
}
