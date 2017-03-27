package li.strolch.report;

import java.util.AbstractMap.SimpleEntry;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

public class ReportElement {

	private Set<String> columnKeys;
	private Function<String, String> columnGetter;

	public ReportElement(Set<String> columnKeys, Function<String, String> columnGetter) {
		super();
		this.columnGetter = columnGetter;
		this.columnKeys = columnKeys;
	}

	public Set<String> getColumnKeys() {
		return this.columnKeys;
	}

	public String getColumn(String key) {
		return this.columnGetter.apply(key);
	}

	public Stream<SimpleEntry<String, String>> keyValueStream() {
		return this.columnKeys.stream().map(k -> new SimpleEntry<>(k, this.columnGetter.apply(k)));
	}

	public Stream<String> valueStream() {
		return this.columnKeys.stream().map(k -> this.columnGetter.apply(k));
	}
}
