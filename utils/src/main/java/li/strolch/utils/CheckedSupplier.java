package li.strolch.utils;

@FunctionalInterface
public interface CheckedSupplier<T> {
	T get() throws Exception;
}
