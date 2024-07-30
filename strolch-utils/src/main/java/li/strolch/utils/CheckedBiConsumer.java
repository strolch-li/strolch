package li.strolch.utils;

@FunctionalInterface
public interface CheckedBiConsumer<T, U> {
	void accept(T t, U u) throws Exception;
}
