package li.strolch.privilege.handler;

import java.util.Map;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.User;

public interface SingleSignOnHandler {

	/**
	 * Initialize the concrete {@link SingleSignOnHandler}. The passed parameter map contains any configuration the
	 * concrete {@link SingleSignOnHandler} might need
	 *
	 * @param parameterMap
	 * 		a map containing configuration properties
	 */
	void initialize(Map<String, String> parameterMap);

	/**
	 * Authenticates a user on a remote Single Sign On service.
	 *
	 * @param data
	 * 		the data required to sign on the user
	 *
	 * @return the user, configured with the remote
	 *
	 * @throws PrivilegeException
	 * 		if the SSO can not be performed with the given data
	 */
	User authenticateSingleSignOn(Object data) throws PrivilegeException;
}
