package li.strolch.agent.api;

import li.strolch.model.StrolchRootElement;
import li.strolch.utils.collections.MapOfLists;

public class ObserverEvent {
	public MapOfLists<String, StrolchRootElement> added = new MapOfLists<>();
	public MapOfLists<String, StrolchRootElement> updated = new MapOfLists<>();
	public MapOfLists<String, StrolchRootElement> removed = new MapOfLists<>();
}
