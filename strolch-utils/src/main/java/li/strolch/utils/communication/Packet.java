package li.strolch.utils.communication;

import static li.strolch.utils.helper.StringHelper.toHexString;

import com.google.gson.JsonObject;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;

public class Packet {

	private final long timestamp;
	private final String command;
	private byte position;
	private PacketState packetState;
	private byte[] sent;
	private byte[] received;

	public Packet(String command, byte position) {
		this(System.currentTimeMillis(), command, position);
	}

	public Packet(long timestamp, String command, byte position) {
		this.timestamp = timestamp;
		this.command = command;
		this.position = position;
		this.packetState = PacketState.New;
	}

	public long getTimestamp() {
		return this.timestamp;
	}

	public String getCommand() {
		return this.command;
	}

	public byte getPosition() {
		return this.position;
	}

	public void setPosition(byte position) {
		this.position = position;
	}

	public PacketState getState() {
		return this.packetState;
	}

	public void setState(PacketState packetState) {
		this.packetState = packetState;
	}

	public Packet state(PacketState packetState) {
		this.packetState = packetState;
		return this;
	}

	public byte[] getSent() {
		return this.sent;
	}

	public void setSent(byte[] sent) {
		this.sent = sent;
	}

	public Packet sent(byte[] sent) {
		this.sent = sent;
		return this;
	}

	public byte[] getReceived() {
		return this.received;
	}

	public Packet received(byte[] received) {
		this.received = received;
		return this;
	}

	@Override
	public String toString() {
		return "Packet{" + "timestamp=" + ISO8601.toString(this.timestamp) //
				+ ", command='" + this.command + '\'' //
				+ ", position=" + this.position //
				+ ", packetState=" + this.packetState //
				+ ", sent=" + (this.sent == null ? "[]" : toHexString(this.sent)) //
				+ ", received=" + (this.received == null ? "[]" : toHexString(this.received)) + '}';
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("time", this.timestamp);
		jsonObject.addProperty("msgType", "Packet");
		jsonObject.addProperty("command", this.command);
		jsonObject.addProperty("position", this.position);
		jsonObject.addProperty("packetState", this.packetState.name());
		if (this.sent != null)
			jsonObject.addProperty("sent", toHexString(this.sent));
		if (this.received != null)
			jsonObject.addProperty("received", toHexString(this.received));

		return jsonObject;
	}

	public static Packet valueOf(JsonObject jsonObject) {

		long time = jsonObject.get("time").getAsLong();
		String command = jsonObject.get("command").getAsString();
		byte position = jsonObject.get("position").getAsByte();
		PacketState packetState = PacketState.valueOf(jsonObject.get("packetState").getAsString());

		Packet packet = new Packet(time, command, position);
		packet.setState(packetState);

		if (jsonObject.has("sent")) {
			byte[] sent = StringHelper.fromHexString(jsonObject.get("sent").getAsString());
			packet.setSent(sent);
		}
		if (jsonObject.has("received")) {
			byte[] received = StringHelper.fromHexString(jsonObject.get("received").getAsString());
			packet.received(received);
		}

		return packet;
	}
}
