package li.strolch.privilege.model.internal;

import li.strolch.privilege.model.Usage;
import li.strolch.utils.dbc.DBC;

import java.time.LocalDateTime;

public final class UserChallenge {
	private final User user;
	private final String challenge;
	private final String source;
	private final LocalDateTime initiated;
	private final Usage usage;
	private boolean fulfilled;

	public UserChallenge(Usage usage, User user, String challenge, String source) {
		DBC.PRE.assertNotNull("usage may not be null", usage);
		DBC.PRE.assertNotNull("user may not be null", user);
		DBC.PRE.assertNotNull("challenge may not be empty", challenge);
		DBC.PRE.assertNotNull("source may not be empty", source);
		this.usage = usage;
		this.user = user;
		this.challenge = challenge;
		this.source = source;
		this.initiated = LocalDateTime.now();
	}

	public Usage getUsage() {
		return this.usage;
	}

	public User getUser() {
		return this.user;
	}

	public String getChallenge() {
		return this.challenge;
	}

	public String getSource() {
		return this.source;
	}

	public LocalDateTime getInitiated() {
		return this.initiated;
	}

	public boolean isFulfilled() {
		return this.fulfilled;
	}

	public void fulfilled() {
		this.fulfilled = true;
	}
}