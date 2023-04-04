package li.strolch.utils;

@FunctionalInterface
public interface CheckedFunction<T, R> {
	R apply(T t);
}