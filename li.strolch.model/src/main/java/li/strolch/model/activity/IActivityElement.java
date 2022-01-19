/*
 * Copyright 2015 Martin Smock <martin.smock@bluewin.ch>
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
package li.strolch.model.activity;

import java.util.function.Predicate;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.*;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * Marker for all child elements of {@link Activity} objects
 *
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public interface IActivityElement extends StrolchElement {

	/**
	 * @return the start time of this element
	 */
	Long getStart();

	/**
	 * @return the end time of this element
	 */
	Long getEnd();

	/**
	 * @return the {@link State} of this element
	 */
	State getState();

	/**
	 * @return true if this element is an {@link Action}
	 */
	boolean isAction();

	/**
	 * @return true if this element is an {@link Activity}
	 */
	boolean isActivity();

	/**
	 * Set the parent
	 *
	 * @param activity
	 * 		the activity to set as parent
	 */
	void setParent(Activity activity);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter} on the {@link ParameterBag} with the id {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or otherwise queries its parent, until the root element is reached.
	 * </p>
	 *
	 * <p>
	 * If the parameter does not exist, null is returned
	 * </p>
	 *
	 * @see GroupedParameterizedElement#getRelationParam(String)
	 */
	<U, T extends Parameter<U>> T findRelationParam(String paramKey);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter} on the {@link ParameterBag} with the id {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or otherwise queries its parent, until the root element is reached.
	 * </p>
	 *
	 * <p>
	 * If the parameter does not exist and {@code assertExists} is true, then a {@link StrolchModelException} is thrown
	 * </p>
	 *
	 * @see GroupedParameterizedElement#getRelationParam(String, boolean)
	 */
	<U, T extends Parameter<U>> T findRelationParam(String paramKey, boolean assertExists);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 *
	 * <p>
	 * If the parameter does not exist, null is returned
	 * </p>
	 *
	 * @see GroupedParameterizedElement#getParameter(String, String)
	 */
	<U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey);

	/**
	 * <p>
	 * Checks if this element contains the {@link Parameter}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 *
	 * <p>
	 * If the parameter does not exist and {@code assertExists} is true, then a {@link StrolchModelException} is thrown
	 * </p>
	 */
	<U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey, boolean assertExists)
			throws StrolchModelException;

	/**
	 * <p>
	 * Checks if this element contains the {@link PolicyDef}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 *
	 * <p>
	 * If the policy def does not exist and no default policy is passed, then a {@link StrolchModelException} is thrown
	 * </p>
	 *
	 * @param className
	 * 		the class name of the policy to find
	 * @param defaultDef
	 * 		default {@link PolicyDef} to return if not found
	 *
	 * @throws StrolchModelException
	 * 		if no default policy, and policy not found
	 */
	PolicyDef findPolicy(String className, PolicyDef defaultDef) throws StrolchModelException;

	/**
	 * <p>
	 * Checks if this element contains the {@link PolicyDef}, or otherwise queries its parent, until the root element is
	 * reached.
	 * </p>
	 *
	 * <p>
	 * If the policy def does not exist and no default policy is passed, then a {@link StrolchModelException} is thrown
	 * </p>
	 *
	 * @param clazz
	 * 		the class of the policy to find
	 * @param defaultDef
	 * 		default {@link PolicyDef} to return if not found
	 *
	 * @throws StrolchModelException
	 * 		if no default policy, and policy not found
	 */
	PolicyDef findPolicy(Class<?> clazz, PolicyDef defaultDef) throws StrolchModelException;

	@Override
	Activity getParent();

	@Override
	Activity getRootElement();

	@Override
	IActivityElement getClone();

	/**
	 * Implements the visitor pattern. Concrete implementation will call the proper method on the visitor
	 *
	 * @param visitor
	 * 		the visitor to accept
	 *
	 * @return the result of the visitor being accepted
	 */
	@Override
	<T> T accept(StrolchElementVisitor<T> visitor);

	default boolean inCreatedPhase() {
		return getState().inCreatedPhase();
	}

	default boolean inPlanningPhase() {
		return getState().inPlanningPhase();
	}

	default boolean inExecutionPhase() {
		return getState().inExecutionPhase();
	}

	default boolean inExecutionPlanningPhase() {
		return getState().inExecutionPlanningPhase();
	}

	default boolean inErrorPhase() {
		return getState().inErrorPhase();
	}

	default boolean inExecutionWarningPhase() {
		return getState().inExecutionWarningPhase();
	}

	default boolean inClosedPhase() {
		return getState().inClosedPhase();
	}

	default Activity findParent(Predicate<Activity> predicate) {

		Activity parent = getParent();
		while (parent != null && !predicate.test(parent))
			parent = parent.getParent();

		if (parent == null)
			throw new StrolchModelException(getLocator() + " has no parent where predicate " + predicate + " is true!");

		return parent;
	}

	/**
	 * Casts this {@link IActivityElement} to {@link Activity}
	 *
	 * @return this {@link IActivityElement} as {@link Activity}
	 */
	default Activity asActivity() {
		return (Activity) this;
	}

	/**
	 * Casts this {@link IActivityElement} to {@link Action}
	 *
	 * @return this {@link IActivityElement} as {@link Action}
	 */
	default Action asAction() {
		return (Action) this;
	}
}
