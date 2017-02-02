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
package li.strolch.model;

@SuppressWarnings("nls")
public class Tags {

	public static final String RESOURCE = "Resource";
	public static final String ORDER = "Order";
	public static final String ACTIVITY = "Activity";

	public static final String CDATA = "CDATA";
	public static final String ID = "Id";
	public static final String NAME = "Name";
	public static final String TYPE = "Type";
	public static final String DATE = "Date";
	public static final String STATE = "State";
	public static final String VALUE = "Value";
	public static final String VALUES = "Values";
	public static final String TIME = "Time";
	public static final String INTERPRETATION = "Interpretation";
	public static final String UOM = "Uom";
	public static final String HIDDEN = "Hidden";
	public static final String INDEX = "Index";
	public static final String PARAMETER = "Parameter";
	public static final String TIMED_STATE = "TimedState";
	public static final String PARAMETERIZED_ELEMENT = "ParameterizedElement";

	public static final String PARAMETER_BAG = "ParameterBag";
	public static final String STROLCH_MODEL = "StrolchModel";
	public static final String INCLUDE_FILE = "IncludeFile";
	public static final String FILE = "file";
	public static final String BAG = "Bag";
	public static final String AUDIT = "Audit";
	public static final String POLICIES = "Policies";
	public static final String POLICY = "Policy";

	public static final String VERSION = "Version";
	public static final String CREATED_AT = "CreatedAt";
	public static final String CREATED_BY = "CreatedBy";
	public static final String DELETED = "Deleted";

	public static final String TIME_ORDERING = "TimeOrdering";
	public static final String ACTION = "Action";
	public static final String START = "Start";
	public static final String END = "End";
	public static final String VALUE_CHANGES = "ValueChanges";
	public static final String VALUE_CHANGE = "ValueChange";
	public static final String RESOURCE_ID = "ResourceId";
	public static final String RESOURCE_TYPE = "ResourceType";
	public static final String STATE_ID = "StateId";

	public static class Json {

		// elements

		public static final String RESOURCE = Tags.RESOURCE;
		public static final String ORDER = Tags.ORDER;
		public static final String ACTIVITY = Tags.ACTIVITY;

		public static final String OBJECT_TYPE = "objectType";

		public static final String ID = "id";
		public static final String TYPE = "type";
		public static final String NAME = "name";
		public static final String DATE = "date";
		public static final String STATE = "state";
		public static final String TIME_ORDERING = "timeOrdering";

		public static final String PARAMETER_BAGS = "parameterBags";
		public static final String PARAMETERS = "parameters";
		public static final String INTERPRETATION = "interpretation";
		public static final String UOM = "uom";
		public static final String HIDDEN = "hidden";
		public static final String INDEX = "index";

		public static final String TIMED_STATES = "timedStates";
		public static final String VALUES = "values";

		public static final String ACTION = "action";
		public static final String RESOURCE_ID = "resourceId";
		public static final String RESOURCE_TYPE = "resourceType";
		public static final String VALUE_CHANGES = "valueChanges";
		public static final String STATE_ID = "stateId";
		public static final String VALUE = "value";
		public static final String TIME = "time";

		public static final String POLICIES = "policies";

		public static final String VERSION = "version";
		public static final String CREATED_AT = "createdAt";
		public static final String CREATED_BY = "createdBy";
		public static final String DELETED = "deleted";

		// miscellaneous

		public static final String ELEMENTS = "elements";
		public static final String NR_OF_ELEMENTS = "nrOfElements";
		public static final String ELEMENT_MAPS = "elementMaps";
		public static final String TYPES = "types";
		public static final String REALMS = "realms";
		public static final String SIZE = "size";
	}

	public static class Audit {
		public static final String ID = Tags.ID;

		public static final String USERNAME = "Username";
		public static final String FIRSTNAME = "Firstname";
		public static final String LASTNAME = "Lastname";
		public static final String DATE = "Date";

		public static final String ELEMENT_TYPE = "ElementType";
		public static final String ELEMENT_SUB_TYPE = "ElementSubType";
		public static final String ELEMENT_ACCESSED = "ElementAccessed";
		public static final String NEW_VERSION = "NewVersion";

		public static final String ACTION = "Action";
		public static final String ACCESS_TYPE = "AccessType";
	}
}
