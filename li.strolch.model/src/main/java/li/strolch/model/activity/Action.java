package li.strolch.model.activity;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.Resource;
import li.strolch.model.State;
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
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Action extends GroupedParameterizedElement implements IActivityElement {

	protected static final long serialVersionUID = 1L;

	protected Long start;
	protected Long end;

	protected String resourceId;

	protected final List<IValueChange<?>> startChanges = new ArrayList<>();
	protected final List<IValueChange<?>> endChanges = new ArrayList<>();

	protected State state = State.CREATED;

	protected Activity parent;

	private String resourceType;

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
	 * @return the current <code>State</code> of the a<code>Action</code>
	 */
	public State getState() {
		return state;
	}

	/**
	 * @param state
	 *            the target <code>State</code> of the a<code>Action</code>
	 */
	public void setState(State state) {
		this.state = state;
	}

	/**
	 * @return the type of the <code>Resource</code> this <code>Action</code>
	 *         acts on
	 */
	public String getResourceType() {
		return this.resourceType;
	}

	/**
	 * @param resourceType
	 */
	public void setResourceType(String resourceType) {
		this.resourceType = resourceType;
	}

	/**
	 * @param add
	 *            <code>IValueChange</code> to be applied to the
	 *            <code>Resource</code> to time <code>Action.getStart()<code>
	 * 
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addStartChange(IValueChange<?> change) {
		return startChanges.add(change);
	}

	/**
	 * @param change
	 *            <code>IValueChange</code> to be applied to the
	 *            <code>Resource</code> at time <code>Action.getEnd()<code>
	 * @return <tt>true</tt> (as specified by {@link Collection#add})
	 */
	public boolean addEndChange(IValueChange<?> change) {
		return endChanges.add(change);
	}

	/**
	 * @return the list of <code>IValueChange</code> attached to the
	 *         <code>Action</code> start
	 */
	public List<IValueChange<?>> getStartChanges() {
		return startChanges;
	}

	/**
	 * @return the list of <code>IValueChange</code> attached to the
	 *         <code>Action</code> end
	 */
	public List<IValueChange<?>> getEndChanges() {
		return endChanges;
	}

	@Override
	public Element toDom(Document doc) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public StrolchElement getParent() {
		return parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return (parent == null) ? null : parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	@Override
	public StrolchElement getClone() {
		Action clone = new Action(getId(), getName(), getType());
		clone.setDbid(getDbid());
		clone.setEnd(end);
		clone.setResourceId(resourceId);
		clone.setResourceType(resourceType);
		clone.setStart(start);
		clone.setState(state);
		for (IValueChange<?> change : getStartChanges()) {
			clone.startChanges.add(change.getClone());
		}
		for (IValueChange<?> change : getEndChanges()) {
			clone.endChanges.add(change.getClone());
		}
		return clone;
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		locatorBuilder.append(this.id);
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
		builder.append(", resourceId=");
		builder.append(this.resourceId);
		builder.append(", state=");
		builder.append(this.state);
		builder.append(", start=");
		builder.append(this.start);
		builder.append(", end=");
		builder.append(this.end);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public void setParent(Activity activity) {
		this.parent = activity;
	}

}
