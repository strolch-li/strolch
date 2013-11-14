package li.strolch.runtime.observer;

import java.util.List;

import li.strolch.model.StrolchElement;

public interface ObserverHandler {

	public void add(String key, List<StrolchElement> elements);

	public void update(String key, List<StrolchElement> elements);

	public void remove(String key, List<StrolchElement> elements);

	public void registerObserver(String key, Observer observer);

	public void unregisterObserver(String key, Observer observer);
}
