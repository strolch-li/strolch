/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model.internal;

/**
 * @author rvonburg
 * 
 */
public final class Session {

	private final String sessionId;
	private final String username;
	private final long loginTime;

	private final String authToken;
	private final String authPassword;

	/**
	 * 
	 * @param sessionId
	 * @param authToken
	 * @param authPassword
	 * @param username
	 * @param loginTime
	 */
	public Session(String sessionId, String authToken, String authPassword, String username, long loginTime) {
		this.sessionId = sessionId;
		this.authToken = authToken;
		this.authPassword = authPassword;
		this.username = username;
		this.loginTime = loginTime;
	}

	/**
	 * @return the sessionId
	 */
	public String getSessionId() {
		return this.sessionId;
	}

	/**
	 * @return the authToken
	 */
	public String getAuthToken() {
		return this.authToken;
	}

	/**
	 * @return the authPassword
	 */
	public String getAuthPassword() {
		return this.authPassword;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @return the loginTime
	 */
	public long getLoginTime() {
		return this.loginTime;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.authPassword == null) ? 0 : this.authPassword.hashCode());
		result = prime * result + ((this.authToken == null) ? 0 : this.authToken.hashCode());
		result = prime * result + (int) (this.loginTime ^ (this.loginTime >>> 32));
		result = prime * result + ((this.sessionId == null) ? 0 : this.sessionId.hashCode());
		result = prime * result + ((this.username == null) ? 0 : this.username.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Session))
			return false;
		Session other = (Session) obj;
		if (this.authPassword == null) {
			if (other.authPassword != null)
				return false;
		} else if (!this.authPassword.equals(other.authPassword))
			return false;
		if (this.authToken == null) {
			if (other.authToken != null)
				return false;
		} else if (!this.authToken.equals(other.authToken))
			return false;
		if (this.loginTime != other.loginTime)
			return false;
		if (this.sessionId == null) {
			if (other.sessionId != null)
				return false;
		} else if (!this.sessionId.equals(other.sessionId))
			return false;
		if (this.username == null) {
			if (other.username != null)
				return false;
		} else if (!this.username.equals(other.username))
			return false;
		return true;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Session [sessionId=");
		builder.append(this.sessionId);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", loginTime=");
		builder.append(this.loginTime);
		builder.append("]");
		return builder.toString();
	}

}
