package li.strolch.privilege.handler;

import li.strolch.privilege.model.internal.User;

public class ConsoleUserChallengeHandler extends UserChallengeHandler {

	@Override
	public void sendChallengeToUser(User user, String challenge) {
		logger.info("Password reset challenge for {} is: {}", user.getUsername(), challenge);
	}
}
