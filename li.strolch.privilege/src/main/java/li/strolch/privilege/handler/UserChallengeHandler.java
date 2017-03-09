package li.strolch.privilege.handler;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserChallenge;
import li.strolch.utils.CodeGenerator;

public abstract class UserChallengeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(ConsoleUserChallengeHandler.class);

	protected Map<User, UserChallenge> challenges;

	/**
	 * Initialize the concrete {@link UserChallengeHandler}. The passed parameter map contains any configuration the
	 * concrete {@link UserChallengeHandler} might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap) {
		this.challenges = new HashMap<>();
	}

	/**
	 * Generates and returns a new challenge
	 * 
	 * @return a new challenge
	 */
	protected String generateChallenge() {
		String challenge = CodeGenerator.alphaNumericUpper(12);
		return challenge;
	}

	/**
	 * Initiate a password reset challenge for the given user
	 * 
	 * @param usage
	 *            the {@link Usage} for this certificate
	 * @param user
	 *            the user for which to initiate the challenge for
	 */
	public synchronized void initiateChallengeFor(Usage usage, User user) {

		String challenge = generateChallenge();
		UserChallenge userChallenge = new UserChallenge(usage, user, challenge);
		this.challenges.put(user, userChallenge);

		sendChallengeToUser(user, challenge);
	}

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
	public synchronized UserChallenge validateResponse(User user, String challenge) throws PrivilegeException {

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
	 * @param challenge
	 */
	public abstract void sendChallengeToUser(User user, String challenge);
}
