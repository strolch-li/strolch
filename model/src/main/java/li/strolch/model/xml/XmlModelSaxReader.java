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

import static li.strolch.model.Tags.*;

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
import li.strolch.model.parameter.TextParameter;
import li.strolch.model.policy.JavaPolicyDef;
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
	private TextParameter textParam;
	private final Deque<Activity> activityStack;
	private ParameterBag pBag;
	private StrolchTimedState<? extends IValue<?>> state;
	private PolicyDefs policies;

	private StringBuilder textBuffer;

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

		switch (qName) {

		case STROLCH_MODEL:
			break;

		case RESOURCE:

			String resId = attributes.getValue(ID);
			String resName = attributes.getValue(NAME);
			String resType = attributes.getValue(TYPE);

			this.parameterizedElement = new Resource(resId, resName, resType);

			break;

		case ACTIVITY:

			String activityId = attributes.getValue(ID);
			String activityName = attributes.getValue(NAME);
			String activityType = attributes.getValue(TYPE);
			String timeOrderingS = attributes.getValue(TIME_ORDERING);
			if (StringHelper.isEmpty(timeOrderingS))
				throw new StrolchException("TimeOrdering is not set for Activity with ID " + activityId);
			TimeOrdering timeOrdering = TimeOrdering.parse(timeOrderingS);
			Activity activity = new Activity(activityId, activityName, activityType, timeOrdering);

			this.parameterizedElement = activity;

			this.activityStack.push(activity);

			break;

		case ACTION:

			String actionId = attributes.getValue(ID);
			String actionName = attributes.getValue(NAME);
			String actionType = attributes.getValue(TYPE);
			String actionResourceId = attributes.getValue(RESOURCE_ID);
			String actionResourceType = attributes.getValue(RESOURCE_TYPE);
			String actionState = attributes.getValue(STATE);
			Action action = new Action(actionId, actionName, actionType);
			action.setResourceId(actionResourceId);
			action.setResourceType(actionResourceType);
			if (StringHelper.isNotEmpty(actionState))
				action.setState(State.parse(actionState));

			this.parameterizedElement = action;

			break;

		case VALUE_CHANGE:

			String valueChangeStateId = attributes.getValue(STATE_ID);
			String valueChangeTimeS = attributes.getValue(TIME);
			String valueChangeValue = attributes.getValue(VALUE);
			String valueChangeType = attributes.getValue(TYPE);

			IValue<?> value = StrolchValueType.parse(valueChangeType).valueInstance(valueChangeValue);
			long valueChangeTime = ISO8601FormatFactory.getInstance().getDateFormat().parse(valueChangeTimeS).getTime();
			ValueChange<IValue<?>> valueChange = new ValueChange<>(valueChangeTime, value, valueChangeStateId);

			((Action) this.parameterizedElement).addChange(valueChange);

			break;

		case ORDER:
			String orderId = attributes.getValue(ID);
			String orderName = attributes.getValue(NAME);
			String orderType = attributes.getValue(TYPE);
			String orderDateS = attributes.getValue(DATE);
			String orderStateS = attributes.getValue(STATE);
			Order order = new Order(orderId, orderName, orderType);
			if (orderDateS != null) {
				Date orderDate = ISO8601FormatFactory.getInstance().getDateFormat().parse(orderDateS);
				order.setDate(orderDate);
			}
			if (StringHelper.isNotEmpty(orderStateS))
				order.setState(State.parse(orderStateS));

			this.parameterizedElement = order;

			break;

		case PARAMETER_BAG:
			String pBagId = attributes.getValue(ID);
			String pBagName = attributes.getValue(NAME);
			String pBagType = attributes.getValue(TYPE);
			this.pBag = new ParameterBag(pBagId, pBagName, pBagType);

			break;

		case PARAMETER:

			String paramId = attributes.getValue(ID);
			try {

				String paramName = attributes.getValue(NAME);
				String paramType = attributes.getValue(TYPE);
				String paramHiddenS = attributes.getValue(HIDDEN);
				String paramIndexS = attributes.getValue(INDEX);

				int index = StringHelper.isEmpty(paramIndexS) ? 0 : Integer.parseInt(paramIndexS);
				boolean paramHidden = !StringHelper.isEmpty(paramHiddenS) && StringHelper.parseBoolean(paramHiddenS);
				String paramUom = attributes.getValue(UOM);
				String paramInterpretation = attributes.getValue(INTERPRETATION);

				StrolchValueType type = StrolchValueType.parse(paramType);

				Parameter<?> param = type.parameterInstance();
				param.setId(paramId);
				param.setName(paramName);

				param.setHidden(paramHidden);
				param.setUom(paramUom);
				param.setInterpretation(paramInterpretation);
				param.setIndex(index);

				if (type != StrolchValueType.TEXT) {
					String paramValue = attributes.getValue(VALUE);
					param.setValueFromString(paramValue);
				} else {
					this.textBuffer = new StringBuilder();
					this.textParam = (TextParameter) param;
				}

				this.pBag.addParameter(param);

			} catch (Exception e) {
				throw new StrolchException(
						"Failed to instantiate parameter " + paramId + " for bag " + this.pBag.getLocator() + " due to "
								+ e.getMessage(), e);
			}

			break;

		case TIMED_STATE:

			String stateId = attributes.getValue(ID);
			try {
				String stateName = attributes.getValue(NAME);
				String stateTypeS = attributes.getValue(TYPE);
				String stateHiddenS = attributes.getValue(HIDDEN);
				String stateIndexS = attributes.getValue(INDEX);
				String stateUom = attributes.getValue(UOM);
				String stateInterpretation = attributes.getValue(INTERPRETATION);
				int stateIndex = StringHelper.isEmpty(stateIndexS) ? 0 : Integer.parseInt(stateIndexS);
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
				throw new StrolchException("Failed to instantiate TimedState " + stateId + " for resource "
						+ this.parameterizedElement.getLocator() + " due to " + e.getMessage(), e);
			}

			break;

		case VALUE:

			String valueTime = attributes.getValue(TIME);
			Date date = ISO8601FormatFactory.getInstance().parseDate(valueTime);
			long time = date.getTime();
			String valueValue = attributes.getValue(VALUE);

			this.state.setStateFromStringAt(time, valueValue);

			break;

		case POLICIES:

			this.policies = new PolicyDefs();

			break;

		case POLICY:

			String policyType = attributes.getValue(TYPE);
			String policyValue = attributes.getValue(VALUE);

			try {

				PolicyDef policyDef = PolicyDef.valueOf(policyType, policyValue);
				if (policyDef instanceof JavaPolicyDef j && !j.isClassExists()) {
					logger.error(
							"Policy invalid for " + policyType + " = " + policyValue + ": class does not exist for "
									+ this.parameterizedElement.getLocator());
				}

				this.policies.addOrUpdate(policyDef);
			} catch (Exception e) {
				throw new StrolchException("Failed to parse policy " + policyType + " = " + policyValue + " for bag "
						+ this.parameterizedElement + " due to " + e.getMessage(), e);
			}

			break;

		case VERSION:

			try {
				String versionS = attributes.getValue(VERSION);
				int v = Integer.parseInt(versionS);
				String createdBy = attributes.getValue(CREATED_BY);

				String updatedBy = attributes.getValue(UPDATED_BY);
				if (updatedBy == null)
					updatedBy = createdBy;

				String createdS;
				createdS = attributes.getValue("CreatedAt");
				if (createdS == null)
					createdS = attributes.getValue(CREATED);
				Date created = ISO8601FormatFactory.getInstance().getDateFormat().parse(createdS);

				String updatedS = attributes.getValue(UPDATED);
				Date updated;
				if (updatedS == null)
					updated = created;
				else
					updated = ISO8601FormatFactory.getInstance().getDateFormat().parse(updatedS);

				String deletedS = attributes.getValue(DELETED);
				boolean deleted = StringHelper.parseBoolean(deletedS);

				Version version = new Version(this.parameterizedElement.getLocator(), v, createdBy, updatedBy, created,
						updated, deleted);
				((StrolchRootElement) this.parameterizedElement).setVersion(version);

			} catch (Exception e) {
				throw new StrolchException(
						"Failed to parse Version element for bag " + this.parameterizedElement + " due to "
								+ e.getMessage(), e);
			}

			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) {
		if (this.textBuffer != null) {
			this.textBuffer.append(ch, start, length);
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) {

		switch (qName) {
		case RESOURCE:
			this.listener.notifyResource((Resource) this.parameterizedElement);
			this.statistics.nrOfResources++;
			this.parameterizedElement = null;

			break;

		case ACTIVITY:

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

		case ORDER:

			this.listener.notifyOrder((Order) this.parameterizedElement);
			this.statistics.nrOfOrders++;
			this.parameterizedElement = null;

			break;

		case ACTION:

			this.activityStack.peek().addElement((Action) parameterizedElement);
			this.parameterizedElement = this.activityStack.peek();

			break;

		case PARAMETER_BAG:

			this.parameterizedElement.addParameterBag(pBag);
			this.pBag = null;

			break;

		case TIMED_STATE:

			((Resource) this.parameterizedElement).addTimedState(this.state);

			break;

		case POLICIES:

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

		case PARAMETER:

			if (this.textParam != null) {
				this.textParam.setValue(this.textBuffer.toString());
				this.textBuffer = null;
				this.textParam = null;
			}

			break;

		case POLICY:
		case VERSION:
		case INCLUDE_FILE:
		case VALUE:
		case VALUE_CHANGE:
		case STROLCH_MODEL:

			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}
}
