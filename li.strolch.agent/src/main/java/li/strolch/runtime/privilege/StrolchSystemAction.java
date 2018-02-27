package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.model.PrivilegeContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link SystemAction} to run {@link PrivilegedRunnable} as a system user
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchSystemAction extends SystemAction {

	public static final Logger logger = LoggerFactory.getLogger(StrolchSystemAction.class);

	private PrivilegedRunnable runnable;

	public StrolchSystemAction(PrivilegedRunnable runnable) {
		this.runnable = runnable;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		try {
			this.runnable.run(privilegeContext);
		} catch (Exception e) {
			logger.error("Failed to execute SystemAction for " + privilegeContext.getUsername() + " due to " + e
					.getMessage(), e);
		}
	}
}
