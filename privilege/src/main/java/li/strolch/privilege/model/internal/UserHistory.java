package li.strolch.privilege.model.internal;

import java.time.ZonedDateTime;

import li.strolch.utils.iso8601.ISO8601;

public class UserHistory {

	private ZonedDateTime firstLogin;
	private ZonedDateTime lastLogin;
	private ZonedDateTime lastPasswordChange;

	public UserHistory() {
		this.firstLogin = ISO8601.EMPTY_VALUE_ZONED_DATE;
		this.lastLogin = ISO8601.EMPTY_VALUE_ZONED_DATE;
		this.lastPasswordChange = ISO8601.EMPTY_VALUE_ZONED_DATE;
	}

	public ZonedDateTime getFirstLogin() {
		return this.firstLogin;
	}

	public boolean isFirstLoginEmpty() {
		return this.firstLogin.equals(ISO8601.EMPTY_VALUE_ZONED_DATE);
	}

	public void setFirstLogin(ZonedDateTime firstLogin) {
		this.firstLogin = firstLogin;
	}

	public ZonedDateTime getLastLogin() {
		return this.lastLogin;
	}

	public boolean isLastLoginEmpty() {
		return this.lastLogin.equals(ISO8601.EMPTY_VALUE_ZONED_DATE);
	}

	public void setLastLogin(ZonedDateTime lastLogin) {
		this.lastLogin = lastLogin;
	}

	public ZonedDateTime getLastPasswordChange() {
		return this.lastPasswordChange;
	}

	public boolean isLastPasswordChangeEmpty() {
		return this.lastPasswordChange.equals(ISO8601.EMPTY_VALUE_ZONED_DATE);
	}

	public void setLastPasswordChange(ZonedDateTime lastPasswordChange) {
		this.lastPasswordChange = lastPasswordChange;
	}

	public boolean isEmpty() {
		return isFirstLoginEmpty() && isLastLoginEmpty() && isLastPasswordChangeEmpty();
	}

	public UserHistory getClone() {
		UserHistory clone = new UserHistory();
		clone.firstLogin = this.firstLogin;
		clone.lastLogin = this.lastLogin;
		clone.lastPasswordChange = this.lastPasswordChange;
		return clone;
	}
}
