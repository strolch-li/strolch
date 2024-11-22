package li.strolch.privilege.model.internal;

import li.strolch.privilege.model.Privilege;

import java.time.ZonedDateTime;
import java.util.Map;

public record AccessToken(String tokenId, String username, PasswordCrypt passwordCrypt, ZonedDateTime validFrom,
						  ZonedDateTime validTo, Map<String, Privilege> privileges) {

	public AccessToken(String tokenId, String username, PasswordCrypt passwordCrypt, ZonedDateTime validFrom,
			ZonedDateTime validTo, Map<String, Privilege> privileges) {
		this.tokenId = tokenId;
		this.username = username;
		this.passwordCrypt = passwordCrypt;
		this.validFrom = validFrom;
		this.validTo = validTo;
		this.privileges = Map.copyOf(privileges);
	}

	@Override
	public String toString() {
		return "AccessToken{tokenId='%s', username='%s', validFrom=%s, validTo=%s, privileges=%s}".formatted(tokenId,
				username, validFrom, validTo, privileges.size());
	}
}
