package li.strolch.agent.api;

import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.MapOfLists;

import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;

public class ObserverEvent {
	public final MapOfLists<String, StrolchRootElement> added = new MapOfLists<>();
	public final MapOfLists<String, StrolchRootElement> updated = new MapOfLists<>();
	public final MapOfLists<String, StrolchRootElement> removed = new MapOfLists<>();

	@Override
	public String toString() {
		String added = this.added.isEmpty() ? "0" : collectSizes(this.added);
		String updated = this.updated.isEmpty() ? "0" : collectSizes(this.updated);
		String removed = this.removed.isEmpty() ? "0" : collectSizes(this.removed);
		return "ObserverEvent{added=" + added + ", updated=" + updated + ", removed=" + removed + "}";
	}

	private String collectSizes(MapOfLists<String, StrolchRootElement> added) {
		return added.stream()
				.map(entry -> entry.getKey() + "=[" + collectSizeByType(entry) + "]")
				.collect(joining(","));
	}

	private static String collectSizeByType(Map.Entry<String, List<StrolchRootElement>> entry) {
		return entry.getValue()
				.stream()
				.collect(groupingBy(StrolchElement::getType))
				.entrySet()
				.stream()
				.map(e -> e.getKey() + "=" + e.getValue().size())
				.collect(joining(","));
	}
}
