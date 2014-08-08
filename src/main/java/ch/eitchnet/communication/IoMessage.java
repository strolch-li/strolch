package ch.eitchnet.communication;

import java.util.Date;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

public class IoMessage {

	private final String id;
	private final CommandKey key;
	private Date updated;
	private State state;
	private String stateMsg;

	public IoMessage(String id, CommandKey key) {
		this.id = id;
		this.key = key;
		this.state = State.CREATED;
		this.stateMsg = StringHelper.DASH;
		this.updated = new Date();
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @return the key
	 */
	public CommandKey getKey() {
		return this.key;
	}

	/**
	 * @return the updated
	 */
	public Date getUpdated() {
		return this.updated;
	}

	/**
	 * @return the state
	 */
	public State getState() {
		return this.state;
	}

	/**
	 * @return the stateMsg
	 */
	public String getStateMsg() {
		return this.stateMsg;
	}

	/**
	 * @param state
	 *            the state
	 * @param stateMsg
	 *            the stateMsg
	 */
	public void setState(State state, String stateMsg) {
		this.state = state;
		this.stateMsg = stateMsg;
		this.updated = new Date();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(this.getClass().getSimpleName() + " [id=");
		builder.append(this.id);
		builder.append(", key=");
		builder.append(this.key);
		builder.append(", updated=");
		builder.append(ISO8601FormatFactory.getInstance().formatDate(this.updated));
		builder.append(", state=");
		builder.append(this.state);
		if (StringHelper.isNotEmpty(this.stateMsg)) {
			builder.append(", stateMsg=");
			builder.append(this.stateMsg);
		}
		builder.append("]");
		return builder.toString();
	}

	public enum State {
		CREATED, // new
		PENDING, // outbound
		ACCEPTED, // inbound
		DONE, // completed
		FAILED, // completed
		FATAL; // not sendable
	}
}
