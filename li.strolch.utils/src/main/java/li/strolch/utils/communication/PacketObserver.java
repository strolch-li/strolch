package li.strolch.utils.communication;

public interface PacketObserver {

	void notify(Packet packet);

	void notifySent(byte[] sent);

	void notifyReceived(byte[] received);

	void notifyInfo(String msg);

	void notifyError(String msg);

	void notifyError(String msg, Throwable e);
}
