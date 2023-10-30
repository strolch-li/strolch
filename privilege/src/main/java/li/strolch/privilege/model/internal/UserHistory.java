package li.strolch.privilege.model.internal;

import java.time.ZonedDateTime;

import static li.strolch.utils.iso8601.ISO8601.EMPTY_VALUE_ZONED_DATE;

public record UserHistory(ZonedDateTime firstLogin, ZonedDateTime lastLogin, ZonedDateTime lastPasswordChange) {

	public static final UserHistory EMPTY = new UserHistory(EMPTY_VALUE_ZONED_DATE, EMPTY_VALUE_ZONED_DATE,
			EMPTY_VALUE_ZONED_DATE);

	public ZonedDateTime getFirstLogin() {
		return this.firstLogin;
	}

	public boolean isFirstLoginEmpty() {
		return this.firstLogin.equals(EMPTY_VALUE_ZONED_DATE);
	}

	public ZonedDateTime getLastLogin() {
		return this.lastLogin;
	}

	public boolean isLastLoginEmpty() {
		return this.lastLogin.equals(EMPTY_VALUE_ZONED_DATE);
	}

	public ZonedDateTime getLastPasswordChange() {
		return this.lastPasswordChange;
	}

	public boolean isLastPasswordChangeEmpty() {
		return this.lastPasswordChange.equals(EMPTY_VALUE_ZONED_DATE);
	}

	public boolean isEmpty() {
		return isFirstLoginEmpty() && isLastLoginEmpty() && isLastPasswordChangeEmpty();
	}

	public UserHistory withFirstLogin(ZonedDateTime firstLogin) {
		return new UserHistory(firstLogin, this.lastLogin, lastPasswordChange);
	}

	public UserHistory withLastLogin(ZonedDateTime lastLogin) {
		return new UserHistory(this.firstLogin, lastLogin, this.lastPasswordChange);
	}

	public UserHistory withLogin(ZonedDateTime lastLogin) {
		return new UserHistory(isFirstLoginEmpty() ? lastLogin : this.firstLogin, lastLogin, this.lastPasswordChange);
	}

	public UserHistory withLastPasswordChange(ZonedDateTime lastPasswordChange) {
		return new UserHistory(this.firstLogin, this.lastLogin, lastPasswordChange);
	}
}
