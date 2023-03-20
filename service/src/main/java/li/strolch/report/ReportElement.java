package li.strolch.report;

import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public class ReportElement {

	private final List<String> columnKeys;
	private final Function<String, String> columnGetter;

	public ReportElement(List<String> columnKeys, Function<String, String> columnGetter) {
		this.columnGetter = columnGetter;
		this.columnKeys = columnKeys;
	}

	public List<String> getColumnKeys() {
		return this.columnKeys;
	}

	public String getColumn(String key) {
		return this.columnGetter.apply(key);
	}

	public Stream<SimpleEntry<String, String>> keyValueStream() {
		return this.columnKeys.stream().map(k -> new SimpleEntry<>(k, this.columnGetter.apply(k)));
	}

	public Stream<String> valueStream() {
		return this.columnKeys.stream().map(this.columnGetter);
	}
}
