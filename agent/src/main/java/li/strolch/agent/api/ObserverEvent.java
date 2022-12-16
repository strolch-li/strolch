package li.strolch.agent.api;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;

import java.util.List;
import java.util.Map;

import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.MapOfLists;

public class ObserverEvent {
	public MapOfLists<String, StrolchRootElement> added = new MapOfLists<>();
	public MapOfLists<String, StrolchRootElement> updated = new MapOfLists<>();
	public MapOfLists<String, StrolchRootElement> removed = new MapOfLists<>();

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
		String sizeByType = entry.getValue()
				.stream()
				.collect(groupingBy(StrolchElement::getType))
				.entrySet()
				.stream()
				.map(e -> e.getKey() + "=" + e.getValue().size())
				.collect(joining(","));
		return sizeByType;
	}
}
