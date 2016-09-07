package li.strolch.privilege.handler;

import java.util.Map;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserChallenge;

public interface UserChallengeHandler {

	/**
	 * Initialize the concrete {@link UserChallengeHandler}. The passed parameter map contains any configuration the
	 * concrete {@link UserChallengeHandler} might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap);

	/**
	 * Initiate a password reset challenge for the given user
	 * 
	 * @param usage
	 *            the {@link Usage} for this certificate
	 * @param user
	 *            the user for which to initiate the challenge for
	 */
	public void initiateChallengeFor(Usage usage, User user);

	/**
	 * Validate the response of a challenge for the given username
	 * 
	 * @param user
	 *            the user for which the challenge is to be validated
	 * @param challenge
	 *            the challenge from the user
	 * 
	 * @throws PrivilegeException
	 *             if anything goes wrong
	 * 
	 * @return the challenge
	 */
	public UserChallenge validateResponse(User user, String challenge) throws PrivilegeException;

}
