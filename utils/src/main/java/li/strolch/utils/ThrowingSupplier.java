package li.strolch.utils;

import java.util.function.Supplier;

@FunctionalInterface
public interface ThrowingSupplier<T> extends Supplier<T> {

	@Override
	default T get() {
		try {
			return getThrows();
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	T getThrows();
}
