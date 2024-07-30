package li.strolch.utils;

public interface I18nMessageVisitor<T> {

	T visit(I18nMessage message);
}
