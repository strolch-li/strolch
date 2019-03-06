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
package li.strolch.model.visitor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.*;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.utils.dbc.DBC;

/**
 * Visitor of {@link StrolchRootElement} to check if they are equal. This implementations stores a list of {@link
 * Locator} for every element of an object which is not equal, thus making it easy to find the inconsistencies in
 * objects.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementDeepEqualsVisitor implements StrolchElementVisitor<List<Locator>> {

	private List<Locator> mismatchedLocators;
	private StrolchElement srcElement;

	public StrolchElementDeepEqualsVisitor(StrolchElement srcElement) {
		this.srcElement = srcElement;
		this.mismatchedLocators = new ArrayList<>();
	}

	/**
	 * @return the mismatchedLocators
	 */
	public List<Locator> getMismatchedLocators() {
		return this.mismatchedLocators;
	}

	/**
	 * Returns true if the objects are equal, i.e. no locators for mismatches are stored
	 *
	 * @return true if the objects are equal, i.e. no locators for mismatches are stored
	 */
	public boolean isEqual() {
		return this.mismatchedLocators.isEmpty();
	}

	/**
	 * Checks the given orders for deep equality
	 *
	 * @param srcOrder
	 * 		source order
	 * @param dstOrder
	 * 		destination order
	 */
	public void deepEquals(Order srcOrder, Order dstOrder) {
		deepEquals((StrolchRootElement) srcOrder, (StrolchRootElement) dstOrder);

		if (!srcOrder.getState().equals(dstOrder.getState()))
			addLocator(dstOrder.getLocator().append(Tags.STATE));
		if (!srcOrder.getDate().equals(dstOrder.getDate()))
			addLocator(dstOrder.getLocator().append(Tags.DATE));
	}

	/**
	 * Checks the given resources for deep equality
	 *
	 * @param srcRes
	 * 		source resource
	 * @param dstRes
	 * 		destination resource
	 */
	public void deepEquals(Resource srcRes, Resource dstRes) {
		deepEquals((StrolchRootElement) srcRes, (StrolchRootElement) dstRes);

		Set<String> srcTimedStateKeySet = srcRes.getTimedStateKeySet();
		for (String timedStateKey : srcTimedStateKeySet) {
			StrolchTimedState<?> srcTimedState = srcRes.getTimedState(timedStateKey);

			if (!dstRes.hasTimedState(timedStateKey)) {
				addLocator(srcTimedState.getLocator());
				continue;
			}

			StrolchTimedState<?> dstTimedState = dstRes.getTimedState(timedStateKey);
			deepEquals(srcTimedState, dstTimedState);
		}

		Set<String> dstTimedStateKeySet = dstRes.getTimedStateKeySet();
		for (String timedStateKey : dstTimedStateKeySet) {
			if (!srcRes.hasTimedState(timedStateKey)) {
				StrolchTimedState<?> dstTimedState = dstRes.getTimedState(timedStateKey);
				addLocator(dstTimedState.getLocator());
			}
		}
	}

	/**
	 * Checks the given activities for deep equality
	 *
	 * @param srcActivity
	 * 		source activity
	 * @param dstActivity
	 * 		destination activity
	 */
	public void deepEquals(Activity srcActivity, Activity dstActivity) {
		deepEquals((StrolchRootElement) srcActivity, (StrolchRootElement) dstActivity);

		if (!srcActivity.getTimeOrdering().equals(dstActivity.getTimeOrdering()))
			addLocator(dstActivity.getLocator().append(Tags.TIME_ORDERING));

		Iterator<Entry<String, IActivityElement>> iter = srcActivity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement srcActivityElement = iter.next().getValue();

			if (!dstActivity.hasElement(srcActivityElement.getId())) {
				addLocator(srcActivityElement.getLocator());
				continue;
			}

			IActivityElement dstActivityElement = dstActivity.getElement(srcActivityElement.getId());

			if (!srcActivityElement.getClass().equals(dstActivityElement.getClass())) {
				addLocator(srcActivityElement.getLocator());
				continue;
			}

			if (srcActivityElement instanceof Activity)
				deepEquals((Activity) srcActivityElement, (Activity) dstActivityElement);
			else if (srcActivityElement instanceof Action)
				deepEquals((Action) srcActivityElement, (Action) dstActivityElement);
			else
				throw new UnsupportedOperationException("Unhandled instance type " + srcActivityElement.getClass());
		}

		iter = dstActivity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement activityElement = iter.next().getValue();
			if (!srcActivity.hasElement(activityElement.getId()))
				addLocator(activityElement.getLocator());
		}
	}

	private void deepEquals(StrolchRootElement srcElement, StrolchRootElement dstElement) {
		deepEquals((StrolchElement) srcElement, (StrolchElement) dstElement);
		deepEquals((GroupedParameterizedElement) srcElement, (GroupedParameterizedElement) dstElement);

		if (srcElement.hasVersion() && dstElement.hasVersion())
			deepEquals(srcElement.getVersion(), dstElement.getVersion());
		else if (!srcElement.hasVersion() && dstElement.hasVersion())
			addLocator(dstElement.getLocator().append(Tags.VERSION));
		else if (srcElement.hasVersion() && !dstElement.hasVersion())
			addLocator(srcElement.getLocator().append(Tags.VERSION));

		if (srcElement.hasPolicyDefs() && dstElement.hasPolicyDefs())
			deepEquals(srcElement.getPolicyDefs(), dstElement.getPolicyDefs());
		else if (srcElement.hasPolicyDefs() != dstElement.hasPolicyDefs())
			addLocator(srcElement.getPolicyDefs().getLocator());
	}

	private void deepEquals(StrolchElement srcElement, StrolchElement dstElement) {
		DBC.PRE.assertEquals("Both elements should have the same ID", srcElement.getId(), dstElement.getId());
		if (!srcElement.getName().equals(dstElement.getName()))
			addLocator(dstElement.getLocator().append(Tags.NAME));
		if (!srcElement.getType().equals(dstElement.getType()))
			addLocator(dstElement.getLocator().append(Tags.TYPE));
	}

	private void deepEquals(Version srcVersion, Version dstVersion) {
		if (srcVersion.getVersion() != dstVersion.getVersion())
			addLocator(srcVersion.getLocator().append(Tags.VERSION));
		if (!srcVersion.getCreatedBy().equals(dstVersion.getCreatedBy()))
			addLocator(srcVersion.getLocator().append(Tags.VERSION));
		if (!srcVersion.getCreated().equals(dstVersion.getCreated()))
			addLocator(srcVersion.getLocator().append(Tags.VERSION));
		if (!srcVersion.getUpdated().equals(dstVersion.getUpdated()))
			addLocator(srcVersion.getLocator().append(Tags.VERSION));
		if (srcVersion.isDeleted() != dstVersion.isDeleted())
			addLocator(srcVersion.getLocator().append(Tags.VERSION));
	}

	private void deepEquals(PolicyDefs srcPolicyDefs, PolicyDefs dstPolicyDefs) {

		Set<String> srcTypes = srcPolicyDefs.getPolicyTypes();
		for (String srcType : srcTypes) {
			PolicyDef srcPolicyDef = srcPolicyDefs.getPolicyDef(srcType);

			if (!dstPolicyDefs.hasPolicyDef(srcType)) {
				addLocator(dstPolicyDefs.getLocator().append(srcType));
				continue;
			}

			PolicyDef dstPolicyDef = dstPolicyDefs.getPolicyDef(srcType);

			if (srcPolicyDef.getClass() != dstPolicyDef.getClass())
				addLocator(dstPolicyDefs.getLocator().append(srcType));
			if (!srcPolicyDef.getValue().equals(dstPolicyDef.getValue()))
				addLocator(dstPolicyDefs.getLocator().append(srcType));
		}

		Set<String> dstTypes = dstPolicyDefs.getPolicyTypes();
		for (String dstType : dstTypes) {
			if (!srcPolicyDefs.hasPolicyDef(dstType))
				addLocator(srcPolicyDefs.getLocator().append(dstType));
		}
	}

	private void deepEquals(Action srcAction, Action dstAction) {
		deepEquals((StrolchElement) srcAction, (StrolchElement) dstAction);
		deepEquals((GroupedParameterizedElement) srcAction, (GroupedParameterizedElement) dstAction);

		if (!srcAction.getResourceId().equals(dstAction.getResourceId()))
			addLocator(dstAction.getLocator().append(Tags.RESOURCE_ID));
		if (!srcAction.getResourceType().equals(dstAction.getResourceType()))
			addLocator(dstAction.getLocator().append(Tags.RESOURCE_TYPE));
		if (!srcAction.getState().equals(dstAction.getState()))
			addLocator(dstAction.getLocator().append(Tags.STATE));

		if ((srcAction.getParent() == null && srcAction.getParent() != null) || (srcAction.getParent() != null
				&& srcAction.getParent() == null))
			addLocator(dstAction.getLocator());
		else if (!srcAction.getParent().getId().equals(dstAction.getParent().getId()))
			addLocator(dstAction.getLocator());
		else if (!srcAction.getParent().getType().equals(dstAction.getParent().getType()))
			addLocator(dstAction.getLocator());

		if (srcAction.hasChanges() != dstAction.hasChanges())
			addLocator(dstAction.getLocator().append(Tags.VALUE_CHANGES));
		else if (!srcAction.getChanges().equals(dstAction.getChanges()))
			addLocator(dstAction.getLocator().append(Tags.VALUE_CHANGES));

		if (srcAction.hasPolicyDefs() && dstAction.hasPolicyDefs())
			deepEquals(srcAction.getPolicyDefs(), dstAction.getPolicyDefs());
		else if (srcAction.hasPolicyDefs() != dstAction.hasPolicyDefs())
			addLocator(dstAction.getPolicyDefs().getLocator());
	}

	private void deepEquals(GroupedParameterizedElement srcElement, GroupedParameterizedElement dstElement) {
		Set<String> srcBagKeySet = srcElement.getParameterBagKeySet();
		for (String bagKey : srcBagKeySet) {
			ParameterBag srcBag = srcElement.getParameterBag(bagKey);

			if (!dstElement.hasParameterBag(bagKey)) {
				addLocator(srcBag.getLocator());
				continue;
			}

			ParameterBag dstBag = dstElement.getParameterBag(bagKey);
			deepEquals(srcBag, dstBag);
		}

		Set<String> dstBagKeySet = dstElement.getParameterBagKeySet();
		for (String bagKey : dstBagKeySet) {
			if (!srcElement.hasParameterBag(bagKey)) {
				ParameterBag dstBag = dstElement.getParameterBag(bagKey);
				addLocator(dstBag.getLocator());
			}
		}
	}

	private void deepEquals(ParameterBag srcBag, ParameterBag dstBag) {
		deepEquals((StrolchElement) srcBag, (StrolchElement) dstBag);

		Set<String> srcParamKeySet = srcBag.getParameterKeySet();
		for (String paramKey : srcParamKeySet) {
			Parameter<?> srcParam = srcBag.getParameter(paramKey);
			if (!dstBag.hasParameter(paramKey)) {
				addLocator(srcParam.getLocator());
				continue;
			}

			Parameter<?> dstParam = dstBag.getParameter(paramKey);
			deepEquals(srcParam, dstParam);
		}

		Set<String> dstParamKeySet = dstBag.getParameterKeySet();
		for (String paramKey : dstParamKeySet) {
			if (!srcBag.hasParameter(paramKey)) {
				Parameter<?> dstParam = dstBag.getParameter(paramKey);
				addLocator(dstParam.getLocator());
			}
		}
	}

	private void deepEquals(Parameter<?> srcParam, Parameter<?> dstParam) {
		deepEquals((StrolchElement) srcParam, (StrolchElement) dstParam);
		if (!srcParam.getUom().equals(dstParam.getUom()))
			addLocator(dstParam.getLocator());
		if (!srcParam.getInterpretation().equals(dstParam.getInterpretation()))
			addLocator(dstParam.getLocator());
		if (srcParam.isHidden() != dstParam.isHidden())
			addLocator(dstParam.getLocator());
		if (srcParam.getIndex() != dstParam.getIndex())
			addLocator(dstParam.getLocator());

		if (!srcParam.getValue().equals(dstParam.getValue()))
			addLocator(dstParam.getLocator());
	}

	private void deepEquals(StrolchTimedState<?> srcState, StrolchTimedState<?> dstState) {
		deepEquals((StrolchElement) srcState, (StrolchElement) dstState);
		final ITimeVariable<?> srcTimeEvolution = srcState.getTimeEvolution();
		final ITimeVariable<?> dstTimeEvolution = dstState.getTimeEvolution();

		if (!srcTimeEvolution.getValues().equals(dstTimeEvolution.getValues()))
			addLocator(dstState.getLocator().append(Tags.VALUES));
	}

	private void addLocator(Locator locator) {
		this.mismatchedLocators.add(locator);
	}

	public static boolean isEqual(Order srcOrder, Order dstOrder) {
		return srcOrder.accept(new StrolchElementDeepEqualsVisitor(dstOrder)).isEmpty();
	}

	public static boolean isEqual(Resource srcRes, Resource dstRes) {
		return srcRes.accept(new StrolchElementDeepEqualsVisitor(dstRes)).isEmpty();
	}

	public static boolean isEqual(Activity srcAct, Activity dstAct) {
		return srcAct.accept(new StrolchElementDeepEqualsVisitor(dstAct)).isEmpty();
	}

	@Override
	public List<Locator> visitOrder(Order order) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), order.getClass());
		deepEquals((Order) this.srcElement, order);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitResource(Resource resource) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), resource.getClass());
		deepEquals((Resource) this.srcElement, resource);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitActivity(Activity activity) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), activity.getClass());
		deepEquals((Activity) this.srcElement, activity);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitAction(Action action) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), action.getClass());
		deepEquals((Action) this.srcElement, action);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitBooleanParam(BooleanParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitDateParam(DateParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitDurationParam(DurationParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitFloatParam(FloatParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitIntegerParam(IntegerParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitLongParam(LongParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitStringParam(StringParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitStringListParam(StringListParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitIntegerListParam(IntegerListParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitFloatListParam(FloatListParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitLongListParam(LongListParameter param) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), param.getClass());
		deepEquals((Parameter<?>) this.srcElement, param);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitBooleanState(BooleanTimedState state) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), state.getClass());
		deepEquals((StrolchTimedState<?>) this.srcElement, state);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitFloatState(FloatTimedState state) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), state.getClass());
		deepEquals((StrolchTimedState<?>) this.srcElement, state);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitFloatListState(FloatListTimedState state) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), state.getClass());
		deepEquals((StrolchTimedState<?>) this.srcElement, state);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitIntegerState(IntegerTimedState state) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), state.getClass());
		deepEquals((StrolchTimedState<?>) this.srcElement, state);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitStringState(StringSetTimedState state) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), state.getClass());
		deepEquals((StrolchTimedState<?>) this.srcElement, state);
		return getMismatchedLocators();
	}

	@Override
	public List<Locator> visitParameterBag(ParameterBag bag) {
		DBC.PRE.assertEquals("Can't compare apples with pairs =)", this.srcElement.getClass(), bag.getClass());
		deepEquals((ParameterBag) this.srcElement, bag);
		return getMismatchedLocators();
	}
}
