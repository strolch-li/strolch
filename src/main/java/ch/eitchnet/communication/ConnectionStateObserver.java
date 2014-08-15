package ch.eitchnet.communication;

public interface ConnectionStateObserver {

	public void notify(ConnectionState oldState, String oldStateMsg, ConnectionState newState, String newStateMsg);
}
