package li.strolch.websocket;

public class WebSocketRemoteIp {

	private static final ThreadLocal<String> threadLocal = ThreadLocal.withInitial(() -> "notset");

	public static String get() {
		return threadLocal.get();
	}

	public static void set(String remoteIp) {
		threadLocal.set(remoteIp);
	}
}
