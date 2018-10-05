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
package li.strolch.model.xml;

import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.Date;
import java.util.Deque;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlModelSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(XmlModelSaxReader.class);

	protected StrolchElementListener listener;
	protected ModelStatistics statistics;

	private GroupedParameterizedElement parameterizedElement;
	private Deque<Activity> activityStack;
	private ParameterBag pBag;
	private StrolchTimedState<? extends IValue<?>> state;
	private PolicyDefs policies;

	public XmlModelSaxReader(StrolchElementListener listener) {
		this.listener = listener;
		this.statistics = new ModelStatistics();
		this.activityStack = new ArrayDeque<>();
	}

	/**
	 * @return the statistics
	 */
	public ModelStatistics getStatistics() {
		return this.statistics;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		// TODO split each root object into its own file
		switch (qName) {

		case Tags.STROLCH_MODEL:
			break;

		case Tags.RESOURCE:

			String resId = attributes.getValue(Tags.ID);
			String resName = attributes.getValue(Tags.NAME);
			String resType = attributes.getValue(Tags.TYPE);

			this.parameterizedElement = new Resource(resId, resName, resType);

			break;

		case Tags.ACTIVITY:

			String activityId = attributes.getValue(Tags.ID);
			String activityName = attributes.getValue(Tags.NAME);
			String activityType = attributes.getValue(Tags.TYPE);
			String timeOrderingS = attributes.getValue(Tags.TIME_ORDERING);
			if (StringHelper.isEmpty(timeOrderingS))
				throw new StrolchException("TimeOrdering is not set for Activity with ID " + activityId);
			TimeOrdering timeOrdering = TimeOrdering.parse(timeOrderingS);
			Activity activity = new Activity(activityId, activityName, activityType, timeOrdering);

			this.parameterizedElement = activity;

			this.activityStack.push(activity);

			break;

		case Tags.ACTION:

			String actionId = attributes.getValue(Tags.ID);
			String actionName = attributes.getValue(Tags.NAME);
			String actionType = attributes.getValue(Tags.TYPE);
			String actionResourceId = attributes.getValue(Tags.RESOURCE_ID);
			String actionResourceType = attributes.getValue(Tags.RESOURCE_TYPE);
			String actionState = attributes.getValue(Tags.STATE);
			Action action = new Action(actionId, actionName, actionType);
			action.setResourceId(actionResourceId);
			action.setResourceType(actionResourceType);
			if (StringHelper.isNotEmpty(actionState))
				action.setState(State.parse(actionState));

			this.parameterizedElement = action;

			break;

		case Tags.VALUE_CHANGE:

			String valueChangeStateId = attributes.getValue(Tags.STATE_ID);
			String valueChangeTimeS = attributes.getValue(Tags.TIME);
			String valueChangeValue = attributes.getValue(Tags.VALUE);
			String valueChangeType = attributes.getValue(Tags.TYPE);

			IValue<?> value = StrolchValueType.parse(valueChangeType).valueInstance(valueChangeValue);
			long valueChangeTime = ISO8601FormatFactory.getInstance().getDateFormat().parse(valueChangeTimeS).getTime();
			ValueChange<IValue<?>> valueChange = new ValueChange<>(valueChangeTime, value, valueChangeStateId);

			((Action) this.parameterizedElement).addChange(valueChange);

			break;

		case Tags.ORDER:
			String orderId = attributes.getValue(Tags.ID);
			String orderName = attributes.getValue(Tags.NAME);
			String orderType = attributes.getValue(Tags.TYPE);
			String orderDateS = attributes.getValue(Tags.DATE);
			String orderStateS = attributes.getValue(Tags.STATE);
			Order order = new Order(orderId, orderName, orderType);
			if (orderDateS != null) {
				Date orderDate = ISO8601FormatFactory.getInstance().getDateFormat().parse(orderDateS);
				order.setDate(orderDate);
			}
			if (StringHelper.isNotEmpty(orderStateS))
				order.setState(State.parse(orderStateS));

			this.parameterizedElement = order;

			break;

		case Tags.PARAMETER_BAG:
			String pBagId = attributes.getValue(Tags.ID);
			String pBagName = attributes.getValue(Tags.NAME);
			String pBagType = attributes.getValue(Tags.TYPE);
			this.pBag = new ParameterBag(pBagId, pBagName, pBagType);

			break;

		case Tags.PARAMETER:

			String paramId = attributes.getValue(Tags.ID);
			try {

				String paramName = attributes.getValue(Tags.NAME);
				String paramType = attributes.getValue(Tags.TYPE);
				String paramValue = attributes.getValue(Tags.VALUE);
				String paramHiddenS = attributes.getValue(Tags.HIDDEN);
				String paramIndexS = attributes.getValue(Tags.INDEX);

				int index = StringHelper.isEmpty(paramIndexS) ? 0 : Integer.valueOf(paramIndexS);
				boolean paramHidden = !StringHelper.isEmpty(paramHiddenS) && StringHelper.parseBoolean(paramHiddenS);
				String paramUom = attributes.getValue(Tags.UOM);
				String paramInterpretation = attributes.getValue(Tags.INTERPRETATION);

				StrolchValueType type = StrolchValueType.parse(paramType);

				Parameter<?> param = type.parameterInstance();
				param.setId(paramId);
				param.setName(paramName);
				param.setValueFromString(paramValue);

				param.setHidden(paramHidden);
				param.setUom(paramUom);
				param.setInterpretation(paramInterpretation);
				param.setIndex(index);

				this.pBag.addParameter(param);

			} catch (Exception e) {
				throw new StrolchException(
						"Failed to instantiate parameter " + paramId + " for bag " + this.pBag.getLocator() + " due to "
								+ e.getMessage(), e);
			}

			break;

		case Tags.TIMED_STATE:

			String stateId = attributes.getValue(Tags.ID);
			try {
				String stateName = attributes.getValue(Tags.NAME);
				String stateTypeS = attributes.getValue(Tags.TYPE);
				String stateHiddenS = attributes.getValue(Tags.HIDDEN);
				String stateIndexS = attributes.getValue(Tags.INDEX);
				String stateUom = attributes.getValue(Tags.UOM);
				String stateInterpretation = attributes.getValue(Tags.INTERPRETATION);
				int stateIndex = StringHelper.isEmpty(stateIndexS) ? 0 : Integer.valueOf(stateIndexS);
				boolean stateHidden = !StringHelper.isEmpty(stateHiddenS) && StringHelper.parseBoolean(stateHiddenS);

				StrolchValueType stateType = StrolchValueType.parse(stateTypeS);
				this.state = stateType.timedStateInstance();
				this.state.setId(stateId);
				this.state.setName(stateName);
				this.state.setIndex(stateIndex);
				this.state.setHidden(stateHidden);
				this.state.setInterpretation(stateInterpretation);
				this.state.setUom(stateUom);

			} catch (Exception e) {
				throw new StrolchException(
						"Failed to instantiate TimedState " + stateId + " for resource " + this.parameterizedElement
								.getLocator() + " due to " + e.getMessage(), e);
			}

			break;

		case Tags.VALUE:

			String valueTime = attributes.getValue(Tags.TIME);
			Date date = ISO8601FormatFactory.getInstance().parseDate(valueTime);
			long time = date.getTime();
			String valueValue = attributes.getValue(Tags.VALUE);

			this.state.setStateFromStringAt(time, valueValue);

			break;

		case Tags.POLICIES:

			this.policies = new PolicyDefs();

			break;

		case Tags.POLICY:

			String policyType = attributes.getValue(Tags.TYPE);
			String policyValue = attributes.getValue(Tags.VALUE);

			PolicyDef policyDef = PolicyDef.valueOf(policyType, policyValue);
			this.policies.addOrUpdate(policyDef);

			break;

		case Tags.VERSION:

			try {
				String versionS = attributes.getValue(Tags.VERSION);
				int v = Integer.parseInt(versionS);
				String createdBy = attributes.getValue(Tags.CREATED_BY);
				String createdAtS = attributes.getValue(Tags.CREATED_AT);
				Date createdAt = ISO8601FormatFactory.getInstance().parseDate(createdAtS);
				String deletedS = attributes.getValue(Tags.DELETED);
				boolean deleted = StringHelper.parseBoolean(deletedS);

				Version version = new Version(this.parameterizedElement.getLocator(), v, createdBy, createdAt, deleted);
				((StrolchRootElement) this.parameterizedElement).setVersion(version);

			} catch (Exception e) {
				throw new StrolchException(
						"Failed to Version for for bag " + this.parameterizedElement + " due to " + e.getMessage(), e);
			}

			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		switch (qName) {
		case Tags.RESOURCE:
			this.listener.notifyResource((Resource) this.parameterizedElement);
			this.statistics.nrOfResources++;
			this.parameterizedElement = null;

			break;

		case Tags.ACTIVITY:

			Activity activity = this.activityStack.pop();
			if (this.activityStack.isEmpty()) {
				this.listener.notifyActivity(activity);
				this.statistics.nrOfActivities++;
				this.parameterizedElement = null;
			} else {
				this.activityStack.peek().addElement(activity);
				this.parameterizedElement = this.activityStack.peek();
			}

			break;

		case Tags.ORDER:

			this.listener.notifyOrder((Order) this.parameterizedElement);
			this.statistics.nrOfOrders++;
			this.parameterizedElement = null;

			break;

		case Tags.ACTION:

			this.activityStack.peek().addElement((Action) parameterizedElement);
			this.parameterizedElement = this.activityStack.peek();

			break;

		case Tags.PARAMETER_BAG:

			this.parameterizedElement.addParameterBag(pBag);
			this.pBag = null;

			break;

		case Tags.TIMED_STATE:

			((Resource) this.parameterizedElement).addTimedState(this.state);

			break;

		case Tags.POLICIES:

			if (this.parameterizedElement instanceof Resource) {
				((Resource) this.parameterizedElement).setPolicyDefs(this.policies);
			} else if (this.parameterizedElement instanceof Order) {
				((Order) this.parameterizedElement).setPolicyDefs(this.policies);
			} else if (this.parameterizedElement instanceof Activity) {
				((Activity) this.parameterizedElement).setPolicyDefs(this.policies);
			} else if (this.parameterizedElement instanceof Action) {
				((Action) this.parameterizedElement).setPolicyDefs(this.policies);
			} else {
				throw new StrolchPolicyException("Policies are not allowed on " + this.parameterizedElement.getClass());
			}

			this.policies = null;

			break;

		case Tags.POLICY:
		case Tags.VERSION:
		case Tags.PARAMETER:
		case Tags.INCLUDE_FILE:
		case Tags.VALUE:
		case Tags.VALUE_CHANGE:
		case Tags.STROLCH_MODEL:

			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}
}
