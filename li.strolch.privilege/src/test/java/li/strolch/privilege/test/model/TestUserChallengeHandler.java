package li.strolch.privilege.test.model;

import java.util.Map;

import li.strolch.privilege.handler.ConsoleUserChallengeHandler;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserChallenge;

public class TestUserChallengeHandler extends ConsoleUserChallengeHandler {

	private static TestUserChallengeHandler instance;

	public TestUserChallengeHandler() {
		super();
		instance = this;
	}

	public Map<User, UserChallenge> getChallenges() {
		return this.challenges;
	}

	public static TestUserChallengeHandler getInstance() {
		return instance;
	}
}
