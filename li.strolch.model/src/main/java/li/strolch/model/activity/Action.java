package li.strolch.model.activity;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.timevalue.IValueChange;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * An {@link Action} represents a single step within an {@link Activity}, that
 * is, one that is not further decomposed within the {@link Activity}. A
 * {@link Activity} applies {@link IValueChange} objects at the start and end
 * time of the {@link Activity}.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class Action extends GroupedParameterizedElement implements IActivityElement {

	protected static final long serialVersionUID = 1L;

	protected Long start;
	protected Long end; 

	protected String resourceId;

	protected final List<IValueChange<?>> startChanges = new ArrayList<>();
	protected final List<IValueChange<?>> endChanges = new ArrayList<>();
	
	protected ActionState state = ActionState.CREATED; 
	
	protected Activity parent; 

	/**
	 * no argument constructor
	 */
	protected Action() {
		super();
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param type
	 */
	public Action(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * @return the action start time in unix millisecond time
	 */
	public Long getStart() {
		return start;
	}

	/**
	 * @param start
	 *            the action start time in unix millisecond time
	 */
	public void setStart(Long start) {
		this.start = start;
	}

	/**
	 * @return the action end time in unix millisecond time
	 */
	public Long getEnd() {
		return end;
	}

	/**
	 * @param end
	 *            the action end time in unix millisecond time
	 */
	public void setEnd(Long end) {
		this.end = end;
	}

	/**
	 * @return the id of the {@link Resource} the {@link Action} acts on
	 */
	public String getResourceId() {
		return resourceId;
	}

	/**
	 * @param resourceId
	 *            the id of the {@link Resource} the {@link Action} acts on
	 */
	public void setResourceId(String resourceId) {
		this.resourceId = resourceId;
	}

	/**
	 * @param e
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addStartChange(IValueChange<?> change) {
		return startChanges.add(change);
	}

	/**
	 * @param change
	 *            the {@link IValueChange} the action applies at end time
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addEndChange(IValueChange<?> change) {
		return endChanges.add(change);
	}

	public List<IValueChange<?>> getStartChanges() {
		return new ArrayList<IValueChange<?>>(startChanges);
	}

	public List<IValueChange<?>> getEndChanges() {
		return new ArrayList<IValueChange<?>>(endChanges);
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
	public StrolchElement getClone() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Locator getLocator() {
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
		builder.append("Action [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", start=");
		builder.append(this.start);
		builder.append(", end=");
		builder.append(this.end);
		builder.append("]");
		return builder.toString();
	}

	public ActionState getState() {
		return state;
	}

	public void setState(ActionState state) {
		this.state = state;
	}

}
