/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.agent.impl;

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Set;

import li.strolch.agent.api.ComponentState;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.runtime.configuration.StrolchConfigurationException;

public class ComponentController {

	private final StrolchComponent component;
	private final Set<ComponentController> upstreamDependencies;
	private final Set<ComponentController> downstreamDependencies;

	public ComponentController(StrolchComponent component) {
		if (component == null)
			throw new IllegalArgumentException("Component may not be null!");
		this.component = component;
		this.upstreamDependencies = new HashSet<>();
		this.downstreamDependencies = new HashSet<>();
	}

	public ComponentState getState() {
		return this.component.getState();
	}

	public StrolchComponent getComponent() {
		return this.component;
	}

	public String getName() {
		return this.component.getName();
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

		if (equals(controller)) {
			String msg = "{0} can not depend on itself!";
			msg = MessageFormat.format(msg, controller);
			throw new StrolchConfigurationException(msg);
		}

		if (this.upstreamDependencies.contains(controller))
			return;

		validateNoCyclicDependency(controller);

		this.upstreamDependencies.add(controller);
		controller.downstreamDependencies.add(this);
	}

	//  
	//         5
	//         v
	//   2 <-- 11 ---> 10
	//        ^  v     ^
	//        |  9     3
	//        |   ^   /
	//        |    | v
	//       7 --> 8
	//
	// New: 3 > 7

	private void validateNoCyclicDependency(ComponentController controller) {

		if (controller.hasTransitiveUpstreamDependency(this)) {
			String msg = "{0} has transitive upstream dependeny to {1}!";
			msg = MessageFormat.format(msg, this, controller);
			throw new StrolchConfigurationException(msg);
		}
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

	@Override
	public String toString() {
		return "ComponentController [component=" + this.component.getName() + "]";
	}
}
