package li.strolch.model.xml;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

public class StrolchElementListenerToListListener implements StrolchElementListener {

	private final List<StrolchRootElement> elements;

	public StrolchElementListenerToListListener() {
		this.elements = new ArrayList<>();
	}

	@Override
	public void notifyResource(Resource resource) {
		this.elements.add(resource);
	}

	@Override
	public void notifyOrder(Order order) {
		this.elements.add(order);
	}

	@Override
	public void notifyActivity(Activity activity) {
		this.elements.add(activity);
	}

	public Stream<StrolchRootElement> streamElements() {
		return elements.stream();
	}

	public List<StrolchRootElement> getElements() {
		return elements;
	}
}
