package li.strolch.rest.helper;

import jakarta.ws.rs.core.Response;

public class BasicAuthFailure extends Exception {

	private final Response.Status status;
	private final String reason;

	public BasicAuthFailure(Response.Status status, String reason) {
		this.status = status;
		this.reason = reason;
	}

	public BasicAuthFailure(Response.Status status, String reason, Exception cause) {
		super(cause);
		this.status = status;
		this.reason = reason;
	}

	public Response.Status getStatus() {
		return status;
	}

	public String getReason() {
		return reason;
	}
}
