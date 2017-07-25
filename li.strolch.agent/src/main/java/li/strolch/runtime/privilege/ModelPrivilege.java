package li.strolch.runtime.privilege;

import java.text.MessageFormat;

import li.strolch.model.StrolchRootElement;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.privilege.policy.PrivilegePolicyHelper;

public class ModelPrivilege implements PrivilegePolicy {

	/**
	 * The value of {@link Restrictable#getPrivilegeValue()} is used to check if the {@link Role} has this privilege
	 * 
	 * @see li.strolch.privilege.policy.PrivilegePolicy#validateAction(IPrivilege, Restrictable)
	 */
	@Override
	public void validateAction(PrivilegeContext ctx, IPrivilege privilege, Restrictable restrictable)
			throws AccessDeniedException {
		PrivilegePolicyHelper.preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof StrolchRootElement)) {
			String msg = Restrictable.class.getName()
					+ PrivilegeMessages.getString("Privilege.illegalArgument.nonstrolchrootelement"); //$NON-NLS-1$
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return;

		StrolchRootElement rootElement = (StrolchRootElement) object;

		PrivilegePolicyHelper.checkByAllowDenyValues(ctx, privilege, restrictable, rootElement.getType());
	}
}
