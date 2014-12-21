package li.strolch.model.activity;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Parameterized object grouping a collection of {@link Activity} and
 * {@link Action} objects defining the process to be scheduled
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class Activity extends GroupedParameterizedElement implements IActivityElement {

	private static final long serialVersionUID = 1L;

	protected List<IActivityElement> elements = new ArrayList<>();

	public boolean addElement(IActivityElement e) {
		return elements.add(e);
	}

	public List<IActivityElement> getElements() {
		return elements;
	}

	@Override
	public Locator getLocator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Element toDom(Document doc) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public StrolchElement getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public StrolchRootElement getRootElement() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public boolean isRootElement() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public StrolchElement getClone() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		// TODO Auto-generated method stub
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Activity [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", start=");
		builder.append("]");
		return builder.toString();
	}

}
