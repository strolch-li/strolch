package li.strolch.privilege.handler;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserChallenge;

public class ConsoleUserChallengeHandler implements UserChallengeHandler {

	private static final Logger logger = LoggerFactory.getLogger(ConsoleUserChallengeHandler.class);

	protected Map<User, UserChallenge> challenges;

	@Override
	public void initialize(Map<String, String> parameterMap) {
		this.challenges = Collections.synchronizedMap(new HashMap<>());
	}

	@Override
	public void initiateChallengeFor(Usage usage, User user) {
		UserChallenge challenge = new UserChallenge(usage, user, UUID.randomUUID().toString());
		this.challenges.put(user, challenge);
		logger.info("Password reset challenge for " + user.getUsername() + " is: " + challenge.getChallenge());
	}

	@Override
	public UserChallenge validateResponse(User user, String challenge) throws PrivilegeException {

		UserChallenge userChallenge = this.challenges.remove(user);
		if (userChallenge == null)
			throw new PrivilegeException("No challenge exists for user " + user.getUsername());
		if (!userChallenge.getUser().equals(user))
			throw new PrivilegeException("UserChallenge invalid: Wrong user!");

		if (!userChallenge.getChallenge().equals(challenge))
			throw new PrivilegeException("Challenge is invalid!");

		userChallenge.fulfilled();
		return userChallenge;
	}
}
