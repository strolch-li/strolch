package li.strolch.utils;

@FunctionalInterface
public interface CheckedPredicate {
	boolean test() throws Exception;
}
