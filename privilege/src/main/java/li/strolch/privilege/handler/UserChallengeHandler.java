package li.strolch.privilege.handler;

import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserChallenge;
import li.strolch.utils.CodeGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class UserChallengeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(ConsoleUserChallengeHandler.class);

	protected Map<User, UserChallenge> challenges;
	private Map<String, String> parameterMap;

	/**
	 * Returns the configuration for this {@link UserChallengeHandler}
	 *
	 * @return the configuration as a Map
	 */
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	/**
	 * Initialize the concrete {@link UserChallengeHandler}. The passed parameter map contains any configuration the
	 * concrete {@link UserChallengeHandler} might need
	 *
	 * @param parameterMap
	 * 		a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;
		this.challenges = new HashMap<>();
	}

	/**
	 * Generates and returns a new challenge
	 *
	 * @return a new challenge
	 */
	protected String generateChallenge() {
		return CodeGenerator.alphaNumericUpper(12);
	}

	/**
	 * Initiate a password reset challenge for the given user
	 *
	 * @param usage
	 * 		the {@link Usage} for this certificate
	 * @param user
	 * 		the user for which to initiate the challenge for
	 * @param source
	 * 		the source of the challenge initialization
	 */
	public void initiateChallengeFor(Usage usage, User user, String source) {

		String challenge = generateChallenge();
		UserChallenge userChallenge = new UserChallenge(usage, user, challenge, source);
		this.challenges.put(user, userChallenge);

		sendChallengeToUser(user, challenge);
	}

	/**
	 * Validate the response of a challenge for the given username
	 *
	 * @param user
	 * 		the user for which the challenge is to be validated
	 * @param challenge
	 * 		the challenge from the user
	 *
	 * @return the challenge
	 *
	 * @throws PrivilegeException
	 * 		if anything goes wrong
	 */
	public UserChallenge validateResponse(User user, String challenge) throws PrivilegeException {

		// get the challenge
		UserChallenge userChallenge = this.challenges.get(user);
		if (userChallenge == null)
			throw new PrivilegeException("No challenge exists for user " + user.getUsername());

		// validate the challenge
		if (!userChallenge.getUser().equals(user))
			throw new PrivilegeException("UserChallenge invalid: Wrong user!");
		if (!userChallenge.getChallenge().equals(challenge))
			throw new PrivilegeException("Challenge is invalid!");

		// then remove it
		this.challenges.remove(user);

		// it's full filled
		userChallenge.fulfilled();

		return userChallenge;
	}

	/**
	 * Sends the challenge to the user
	 *
	 * @param user
	 * 		the user
	 * @param challenge
	 * 		the challenge
	 */
	public abstract void sendChallengeToUser(User user, String challenge);
}
