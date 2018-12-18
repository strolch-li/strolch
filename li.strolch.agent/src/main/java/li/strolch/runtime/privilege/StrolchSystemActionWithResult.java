package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.handler.SystemActionWithResult;
import li.strolch.privilege.model.PrivilegeContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link SystemAction} to run {@link PrivilegedRunnableWithResult} as a system user
 *
 * @param <T>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchSystemActionWithResult<T> extends SystemActionWithResult<T> {

	public static final Logger logger = LoggerFactory.getLogger(StrolchSystemAction.class);

	private PrivilegedRunnableWithResult<T> runnable;

	public StrolchSystemActionWithResult(PrivilegedRunnableWithResult<T> runnable) {
		this.runnable = runnable;
	}

	@Override
	public T execute(PrivilegeContext privilegeContext) throws Exception {
		try {
			return this.runnable.run(privilegeContext);
		} catch (Exception e) {
			logger.error("Failed to execute SystemAction for " + privilegeContext.getUsername() + " due to " + e
					.getMessage(), e);
			throw e;
		}
	}
}
