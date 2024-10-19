package li.strolch.runtime.privilege;

import li.strolch.model.StrolchRootElement;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.i18n.PrivilegeMessages;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.policy.PrivilegePolicy;

import java.text.MessageFormat;

import static li.strolch.privilege.policy.PrivilegePolicyHelper.checkByAllowDenyValues;
import static li.strolch.privilege.policy.PrivilegePolicyHelper.preValidate;

public class ModelPrivilege implements PrivilegePolicy {

	/**
	 * The value of {@link Restrictable#getPrivilegeValue()} is used to check if the {@link Role} has this privilege
	 *
	 * @see li.strolch.privilege.policy.PrivilegePolicy#validateAction(PrivilegeContext, Privilege, Restrictable)
	 */
	@Override
	public void validateAction(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable)
			throws AccessDeniedException {
		validateAction(ctx, privilege, restrictable, true);
	}

	/**
	 * The value of {@link Restrictable#getPrivilegeValue()} is used to check if the {@link Role} has this privilege
	 *
	 * @see li.strolch.privilege.policy.PrivilegePolicy#validateAction(PrivilegeContext, Privilege, Restrictable)
	 */
	@Override
	public boolean hasPrivilege(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable)
			throws PrivilegeException {
		return validateAction(ctx, privilege, restrictable, false);
	}

	protected boolean validateAction(PrivilegeContext ctx, Privilege privilege, Restrictable restrictable,
			boolean assertHasPrivilege) throws AccessDeniedException {

		preValidate(privilege, restrictable);

		// get the value on which the action is to be performed
		Object object = restrictable.getPrivilegeValue();

		// DefaultPrivilege policy expects the privilege value to be a string
		if (!(object instanceof StrolchRootElement rootElement)) {
			String msg = Restrictable.class.getName() + PrivilegeMessages
					.getString("Privilege.illegalArgument.nonstrolchrootelement");
			msg = MessageFormat.format(msg, restrictable.getClass().getSimpleName());
			throw new PrivilegeException(msg);
		}

		// if everything is allowed, then no need to carry on
		if (privilege.isAllAllowed())
			return true;

		return checkByAllowDenyValues(ctx, privilege, restrictable, rootElement.getType(), assertHasPrivilege);
	}
}
