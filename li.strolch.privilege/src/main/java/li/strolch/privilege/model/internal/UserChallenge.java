package li.strolch.privilege.model.internal;

import java.time.LocalDateTime;

import li.strolch.privilege.model.Usage;

public class UserChallenge {
	private final User user;
	private final String challenge;
	private final String source;
	private final LocalDateTime initiated;
	private boolean fulfilled;
	private Usage usage;

	public UserChallenge(Usage usage, User user, String challenge, String source) {
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