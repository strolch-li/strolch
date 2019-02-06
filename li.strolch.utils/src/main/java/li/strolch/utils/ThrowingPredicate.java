package li.strolch.utils;

@FunctionalInterface
public interface ThrowingPredicate<T> extends Predicate<T> {

	@Override
	default boolean test(T t) {
		try {
			return testThrows(t);
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	boolean testThrows(T t) throws Exception;
}
