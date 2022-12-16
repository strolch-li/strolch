package li.strolch.utils;

@FunctionalInterface
public interface CheckedRunnable {
	void run() throws Exception;
}
