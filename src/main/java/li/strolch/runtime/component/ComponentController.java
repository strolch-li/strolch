package li.strolch.runtime.component;

import java.util.HashSet;
import java.util.Set;

import li.strolch.runtime.configuration.StrolchConfigurationException;

public class ComponentController {

	private StrolchComponent component;
	private Set<ComponentController> upstreamDependencies;
	private Set<ComponentController> downstreamDependencies;

	public ComponentController(StrolchComponent component) {
		if (component == null)
			throw new IllegalArgumentException("Component may not be null!"); //$NON-NLS-1$
		this.component = component;
		this.upstreamDependencies = new HashSet<>();
		this.downstreamDependencies = new HashSet<>();
	}

	public StrolchComponent getComponent() {
		return this.component;
	}

	public Set<ComponentController> getUpstreamDependencies() {
		return new HashSet<>(this.upstreamDependencies);
	}

	public Set<ComponentController> getDownstreamDependencies() {
		return new HashSet<>(this.downstreamDependencies);
	}

	public boolean hasUpstreamDependencies() {
		return !this.upstreamDependencies.isEmpty();
	}

	public boolean hasDownstreamDependencies() {
		return !this.downstreamDependencies.isEmpty();
	}

	public void addUpstreamDependency(ComponentController controller) {

		if (this.equals(controller))
			throw new StrolchConfigurationException(controller + " can not depend on itself!");

		if (this.upstreamDependencies.contains(controller))
			return;

		if (hasTransitiveUpstreamDependency(controller))
			throw new StrolchConfigurationException(this + " has transitive upstream dependeny to " + controller + "!");

		if (hasTransitiveDownstreamDependency(controller))
			throw new StrolchConfigurationException(this + " has transitive downstream dependeny to " + controller
					+ "!");

		this.upstreamDependencies.add(controller);
		controller.downstreamDependencies.add(this);
	}

	public boolean hasUpstreamDependency(ComponentController controller) {
		return this.upstreamDependencies.contains(controller);
	}

	public boolean hasDownstreamDependency(ComponentController controller) {
		return this.downstreamDependencies.contains(controller);
	}

	public boolean hasTransitiveDownstreamDependency(ComponentController controller) {

		if (hasDownstreamDependency(controller))
			return true;

		for (ComponentController downstream : this.downstreamDependencies) {

			if (downstream.hasTransitiveDownstreamDependency(controller))
				return true;
		}

		return false;
	}

	public boolean hasTransitiveUpstreamDependency(ComponentController controller) {

		if (hasUpstreamDependency(controller))
			return true;

		for (ComponentController upstream : this.upstreamDependencies) {

			if (upstream.hasTransitiveUpstreamDependency(controller))
				return true;
		}

		return false;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ComponentController [component=");
		builder.append(this.component.getName());
		builder.append("]");
		return builder.toString();
	}
}
