package li.strolch.utils.communication;

public enum PacketState {
	New,
	Sent,
	Failed,
	NoResponse,
	Retry,
	Done
}