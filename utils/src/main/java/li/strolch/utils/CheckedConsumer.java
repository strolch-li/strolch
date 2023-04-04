package li.strolch.utils;

@FunctionalInterface
public interface CheckedConsumer<T> {
	void accept(T t);
}
