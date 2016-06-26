package li.strolch.communication;

public interface ConnectionStateObserver {

	public void notify(ConnectionState oldState, String oldStateMsg, ConnectionState newState, String newStateMsg);
}
