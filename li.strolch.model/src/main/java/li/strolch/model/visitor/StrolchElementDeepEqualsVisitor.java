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

import ch.eitchnet.utils.dbc.DBC;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.policy.PolicyDef;
import li.strolch.policy.PolicyDefs;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class StrolchElementDeepEqualsVisitor {

	private List<Locator> mismatchedLocators;

	public StrolchElementDeepEqualsVisitor() {
		this.mismatchedLocators = new ArrayList<>();
	}

	/**
	 * @return the mismatchedLocators
	 */
	public List<Locator> getMismatchedLocators() {
		return this.mismatchedLocators;
	}

	public boolean isEqual() {
		return this.mismatchedLocators.isEmpty();
	}

	protected void deepEquals(StrolchElement srcElement, StrolchElement dstElement) {
		DBC.PRE.assertEquals("Both elements should have the same ID", srcElement.getId(), dstElement.getId());
		if (!srcElement.getName().equals(dstElement.getName())) {
			this.mismatchedLocators.add(dstElement.getLocator());
		}
		if (!srcElement.getType().equals(dstElement.getType())) {
			this.mismatchedLocators.add(dstElement.getLocator());
		}
	}

	protected void deepEquals(Order srcOrder, Order dstOrder) {
		deepEquals((StrolchElement) srcOrder, (StrolchElement) dstOrder);
		if (!srcOrder.getState().equals(dstOrder.getState())) {
			this.mismatchedLocators.add(dstOrder.getLocator());
		}
		if (!srcOrder.getDate().equals(dstOrder.getDate())) {
			this.mismatchedLocators.add(dstOrder.getLocator());
		}

		deepEquals((GroupedParameterizedElement) srcOrder, (GroupedParameterizedElement) dstOrder);

		if (srcOrder.hasPolicyDefs() && dstOrder.hasPolicyDefs())
			deepEquals(srcOrder.getPolicyDefs(), dstOrder.getPolicyDefs());
		else if (srcOrder.hasPolicyDefs() != dstOrder.hasPolicyDefs())
			this.mismatchedLocators.add(srcOrder.getLocator());
	}

	private void deepEquals(PolicyDefs srcPolicyDefs, PolicyDefs dstPolicyDefs) {

		Set<String> srcTypes = srcPolicyDefs.getPolicyTypes();
		for (String srcType : srcTypes) {
			PolicyDef srcPolicyDef = srcPolicyDefs.getPolicyDef(srcType);

			if (!dstPolicyDefs.hasPolicyDef(srcType)) {
				this.mismatchedLocators.add(dstPolicyDefs.getParent().getLocator());
				continue;
			}

			PolicyDef dstPolicyDef = dstPolicyDefs.getPolicyDef(srcType);

			if (srcPolicyDef.getClass() != dstPolicyDef.getClass())
				this.mismatchedLocators.add(dstPolicyDefs.getParent().getLocator());
			if (!srcPolicyDef.getValue().equals(dstPolicyDef.getValue()))
				this.mismatchedLocators.add(dstPolicyDefs.getParent().getLocator());
		}

		Set<String> dstTypes = dstPolicyDefs.getPolicyTypes();
		for (String dstType : dstTypes) {
			if (!srcPolicyDefs.hasPolicyDef(dstType)) {
				this.mismatchedLocators.add(srcPolicyDefs.getParent().getLocator());
			}
		}
	}

	protected void deepEquals(Resource srcRes, Resource dstRes) {
		deepEquals((StrolchElement) srcRes, (StrolchElement) dstRes);
		deepEquals((GroupedParameterizedElement) srcRes, (GroupedParameterizedElement) dstRes);

		if (srcRes.hasPolicyDefs() && dstRes.hasPolicyDefs())
			deepEquals(srcRes.getPolicyDefs(), dstRes.getPolicyDefs());
		else if (srcRes.hasPolicyDefs() != dstRes.hasPolicyDefs())
			this.mismatchedLocators.add(srcRes.getLocator());

		Set<String> srcTimedStateKeySet = srcRes.getTimedStateKeySet();
		for (String timedStateKey : srcTimedStateKeySet) {
			StrolchTimedState<?> srcTimedState = srcRes.getTimedState(timedStateKey);

			if (!dstRes.hasTimedState(timedStateKey)) {
				this.mismatchedLocators.add(srcTimedState.getLocator());
				continue;
			}

			StrolchTimedState<?> dstTimedState = dstRes.getTimedState(timedStateKey);
			deepEquals(srcTimedState, dstTimedState);
		}

		Set<String> dstTimedStateKeySet = dstRes.getTimedStateKeySet();
		for (String timedStateKey : dstTimedStateKeySet) {
			if (!srcRes.hasTimedState(timedStateKey)) {
				StrolchTimedState<?> dstTimedState = dstRes.getTimedState(timedStateKey);
				this.mismatchedLocators.add(dstTimedState.getLocator());
			}
		}
	}

	protected void deepEquals(Activity srcActivity, Activity dstActivity) {
		deepEquals((StrolchElement) srcActivity, (StrolchElement) dstActivity);
		deepEquals((GroupedParameterizedElement) srcActivity, (GroupedParameterizedElement) dstActivity);

		if (srcActivity.hasPolicyDefs() && dstActivity.hasPolicyDefs())
			deepEquals(srcActivity.getPolicyDefs(), dstActivity.getPolicyDefs());
		else if (srcActivity.hasPolicyDefs() != dstActivity.hasPolicyDefs())
			this.mismatchedLocators.add(srcActivity.getLocator());

		Iterator<Entry<String, IActivityElement>> iter = srcActivity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement srcActivityElement = iter.next().getValue();

			if (!dstActivity.hasElement(srcActivityElement.getId())) {
				this.mismatchedLocators.add(srcActivityElement.getLocator());
				continue;
			}

			IActivityElement dstActivityElement = dstActivity.getElement(srcActivityElement.getId());

			if (!srcActivityElement.getClass().equals(dstActivityElement.getClass())) {
				this.mismatchedLocators.add(srcActivityElement.getLocator());
				continue;
			}

			if (srcActivityElement instanceof Activity) {
				deepEquals((Activity) srcActivityElement, (Activity) dstActivityElement);
			} else if (srcActivityElement instanceof Action) {
				deepEquals((Action) srcActivityElement, (Action) dstActivityElement);
			} else {
				throw new UnsupportedOperationException("Unhandled instance type " + srcActivityElement.getClass());
			}
		}

		iter = dstActivity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement activityElement = iter.next().getValue();
			if (!srcActivity.hasElement(activityElement.getId())) {
				this.mismatchedLocators.add(activityElement.getLocator());
			}
		}
	}

	protected void deepEquals(Action srcAction, Action dstAction) {
		deepEquals((StrolchElement) srcAction, (StrolchElement) dstAction);
		deepEquals((GroupedParameterizedElement) srcAction, (GroupedParameterizedElement) dstAction);

		if (!srcAction.getResourceId().equals(dstAction.getResourceId())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		}
		if (!srcAction.getResourceType().equals(dstAction.getResourceType())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		}
		if (!srcAction.getState().equals(dstAction.getState())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		}

		if ((srcAction.getParent() == null && srcAction.getParent() != null)
				|| (srcAction.getParent() != null && srcAction.getParent() == null)) {
			this.mismatchedLocators.add(dstAction.getLocator());
		} else if (!srcAction.getParent().getId().equals(dstAction.getParent().getId())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		} else if (!srcAction.getParent().getType().equals(dstAction.getParent().getType())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		}

		if (srcAction.hasChanges() != dstAction.hasChanges()) {
			this.mismatchedLocators.add(dstAction.getLocator());
		} else if (!srcAction.getChanges().equals(dstAction.getChanges())) {
			this.mismatchedLocators.add(dstAction.getLocator());
		}
	}

	protected void deepEquals(GroupedParameterizedElement srcElement, GroupedParameterizedElement dstElement) {
		Set<String> srcBagKeySet = srcElement.getParameterBagKeySet();
		for (String bagKey : srcBagKeySet) {
			ParameterBag srcBag = srcElement.getParameterBag(bagKey);

			if (!dstElement.hasParameterBag(bagKey)) {
				this.mismatchedLocators.add(srcBag.getLocator());
				continue;
			}

			ParameterBag dstBag = dstElement.getParameterBag(bagKey);
			deepEquals(srcBag, dstBag);
		}

		Set<String> dstBagKeySet = dstElement.getParameterBagKeySet();
		for (String bagKey : dstBagKeySet) {
			if (!srcElement.hasParameterBag(bagKey)) {
				ParameterBag dstBag = dstElement.getParameterBag(bagKey);
				this.mismatchedLocators.add(dstBag.getLocator());
			}
		}
	}

	protected void deepEquals(ParameterBag srcBag, ParameterBag dstBag) {
		deepEquals((StrolchElement) srcBag, (StrolchElement) dstBag);

		Set<String> srcParamKeySet = srcBag.getParameterKeySet();
		for (String paramKey : srcParamKeySet) {
			Parameter<?> srcParam = srcBag.getParameter(paramKey);
			if (!dstBag.hasParameter(paramKey)) {
				this.mismatchedLocators.add(srcParam.getLocator());
				continue;
			}

			Parameter<?> dstParam = dstBag.getParameter(paramKey);
			deepEquals(srcParam, dstParam);
		}

		Set<String> dstParamKeySet = dstBag.getParameterKeySet();
		for (String paramKey : dstParamKeySet) {
			if (!srcBag.hasParameter(paramKey)) {
				Parameter<?> dstParam = dstBag.getParameter(paramKey);
				this.mismatchedLocators.add(dstParam.getLocator());
			}
		}
	}

	protected void deepEquals(Parameter<?> srcParam, Parameter<?> dstParam) {
		deepEquals((StrolchElement) srcParam, (StrolchElement) dstParam);
		if (!srcParam.getUom().equals(dstParam.getUom())) {
			this.mismatchedLocators.add(dstParam.getLocator());
		}
		if (!srcParam.getInterpretation().equals(dstParam.getInterpretation())) {
			this.mismatchedLocators.add(dstParam.getLocator());
		}
		if (srcParam.isHidden() != dstParam.isHidden()) {
			this.mismatchedLocators.add(dstParam.getLocator());
		}
		if (srcParam.getIndex() != dstParam.getIndex()) {
			this.mismatchedLocators.add(dstParam.getLocator());
		}

		if (!srcParam.getValue().equals(dstParam.getValue())) {
			this.mismatchedLocators.add(dstParam.getLocator());
		}
	}

	protected void deepEquals(StrolchTimedState<?> srcState, StrolchTimedState<?> dstState) {
		deepEquals((StrolchElement) srcState, (StrolchElement) dstState);
		final ITimeVariable<?> srcTimeEvolution = srcState.getTimeEvolution();
		final ITimeVariable<?> dstTimeEvolution = dstState.getTimeEvolution();

		if (!srcTimeEvolution.getValues().equals(dstTimeEvolution.getValues())) {
			this.mismatchedLocators.add(dstState.getLocator());
		}
	}

	public static boolean isEqual(Order srcOrder, Order dstOrder) {
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		return visitor.isEqual();
	}

	public static boolean isEqual(Resource srcRes, Resource dstRes) {
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		return visitor.isEqual();
	}
}
