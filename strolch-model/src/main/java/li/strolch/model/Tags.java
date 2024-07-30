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

/**
 * This class contains all constants used to marshall strolch objects to XML and JSON.
 */
public class Tags {

	public static final String AGENT = "Agent";

	public static final String RESOURCE = "Resource";
	public static final String ORDER = "Order";
	public static final String ACTIVITY = "Activity";
	public static final String CONTROLLER = "Controller";

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

	public static final String PARAMETER_BAG = "ParameterBag";
	public static final String STROLCH_MODEL = "StrolchModel";
	public static final String INCLUDE_FILE = "IncludeFile";
	public static final String FILE = "file";
	public static final String BAG = "Bag";
	public static final String AUDIT = "Audit";
	public static final String POLICIES = "Policies";
	public static final String POLICY = "Policy";

	public static final String VERSION = "Version";
	public static final String CREATED = "Created";
	public static final String UPDATED = "Updated";
	public static final String CREATED_BY = "CreatedBy";
	public static final String UPDATED_BY = "UpdatedBy";
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

	public static final String USERNAME = "Username";
	public static final String STACK_TRACE = "StackTrace";
	public static final String PROPERTIES = "Properties";
	public static final String PROPERTY = "Property";
	public static final String LOCATOR = "Locator";
	public static final String SEVERITY = "Severity";
	public static final String KEY = "Key";
	public static final String BUNDLE = "Bundle";
	public static final String LOG_MESSAGE = "LogMessage";
	public static final String MESSAGE = "Message";
	public static final String EXCEPTION = "Exception";
	public static final String MESSAGES = "Messages";
	public static final String REALM = "Realm";

	public static class Json {

		// elements

		public static final String RESOURCE = Tags.RESOURCE;
		public static final String ORDER = Tags.ORDER;
		public static final String ACTIVITY = Tags.ACTIVITY;
		public static final String ACTION = Tags.ACTION;

		public static final String OBJECT_TYPE = "objectType";

		public static final String ID = "id";
		public static final String TYPE = "type";
		public static final String NAME = "name";
		public static final String DATE = "date";
		public static final String STATE = "state";
		public static final String START = "start";
		public static final String END = "end";
		public static final String TIME_ORDERING = "timeOrdering";

		public static final String PARAMETER_BAGS = "parameterBags";
		public static final String PARAMETERS = "parameters";
		public static final String INTERPRETATION = "interpretation";
		public static final String UOM = "uom";
		public static final String HIDDEN = "hidden";
		public static final String INDEX = "index";

		public static final String TIMED_STATES = "timedStates";
		public static final String VALUES = "values";

		public static final String RESOURCE_ID = "resourceId";
		public static final String RESOURCE_NAME = "resourceName";
		public static final String RESOURCE_TYPE = "resourceType";
		public static final String VALUE_CHANGES = "valueChanges";
		public static final String STATE_ID = "stateId";
		public static final String VALUE = "value";
		public static final String DEFAULT_VALUE = "defaultValue";
		public static final String TIME = "time";
		public static final String COMMAND = "command";
		public static final String DATA = "data";
		public static final String AGENT = "agent";
		public static final String COMPONENTS = "components";

		public static final String POLICIES = "policies";

		public static final String VERSION = "version";
		public static final String CREATED = "created";
		public static final String UPDATED = "updated";
		public static final String CREATED_BY = "createdBy";
		public static final String UPDATED_BY = "updatedBy";
		public static final String DELETED = "deleted";

		public static final String LOCATOR = "locator";
		public static final String SEVERITY = "severity";
		public static final String KEY = "key";
		public static final String UNUSED = "unused";
		public static final String BUNDLE = "bundle";
		public static final String MESSAGE = "message";
		public static final String MESSAGES = "messages";
		public static final String REALM = "realm";
		public static final String ROLES = "roles";

		// miscellaneous

		public static final String MSG = "msg";
		public static final String EXCEPTION_MSG = "exceptionMsg";
		public static final String THROWABLE = "throwable";
		public static final String I_18_N = "i18n";
		public static final String MSG_TYPE = "msgType";
		public static final String USERNAME = "username";
		public static final String PASSWORD = "password";
		public static final String ELEMENTS = "elements";
		public static final String PROPERTIES = "properties";
		public static final String DEPENDENCIES = "dependencies";
		public static final String NR_OF_ELEMENTS = "nrOfElements";
		public static final String ELEMENT_MAPS = "elementMaps";
		public static final String TYPES = "types";
		public static final String REALMS = "realms";
		public static final String SIZE = "size";
		public static final String EXCEPTION = "exception";
		public static final String FORMAT = "format";
		public static final String FLAT = "flat";
		public static final String WITH_LOCATOR = "withLocator";
		public static final String WITH_VERSION = "withVersion";
		public static final String PARAMS = "params";
		public static final String OPERATION = "operation";
		public static final String EXECUTION_POLICY = "executionPolicy";

		public static final String APP_VERSION = "appVersion";
		public static final String APPLICATION_NAME = "applicationName";
		public static final String SYSTEM_STATE = "systemState";
		public static final String AGENT_VERSION = "agentVersion";
		public static final String COMPONENT_VERSIONS = "componentVersions";
		public static final String ERRORS = "errors";
		public static final String COMPONENT_NAME = "componentName";
		public static final String AGENT_NAME = "agentName";
		public static final String ENVIRONMENT = "environment";
		public static final String LOCALE = "locale";
		public static final String TIMEZONE = "timezone";
		public static final String BUILD_TIMESTAMP = "buildTimestamp";
		public static final String SCM_BRANCH = "scmBranch";
		public static final String SCM_REVISION = "scmRevision";
		public static final String ARTIFACT_VERSION = "artifactVersion";
		public static final String ARTIFACT_ID = "artifactId";
		public static final String GROUP_ID = "groupId";

		public static final String OPERATING_SYSTEM = "operatingSystem";
		public static final String OS_NAME = "osName";
		public static final String OS_ARCH = "osArch";
		public static final String OS_VERSION = "osVersion";
		public static final String JAVA_VENDOR = "javaVendor";
		public static final String JAVA_VERSION = "javaVersion";

		public static final String AVAILABLE_PROCESSORS = "availableProcessors";
		public static final String SYSTEM_LOAD_AVERAGE = "systemLoadAverage";

		public static final String START_TIME = "startTime";
		public static final String UPTIME = "uptime";

		public static final String MEMORY = "memory";
		public static final String TOTAL_PHYSICAL_MEMORY_SIZE = "totalPhysicalMemorySize";
		public static final String FREE_PHYSICAL_MEMORY_SIZE = "freePhysicalMemorySize";
		public static final String FREE_SWAP_SPACE_SIZE = "freeSwapSpaceSize";
		public static final String COMMITTED_VIRTUAL_MEMORY_SIZE = "committedVirtualMemorySize";
		public static final String HEAP_MEMORY_USAGE_INIT = "heapMemoryUsageInit";
		public static final String HEAP_MEMORY_USAGE_USED = "heapMemoryUsageUsed";
		public static final String HEAP_MEMORY_USAGE_MAX = "heapMemoryUsageMax";
		public static final String HEAP_MEMORY_USAGE_COMMITTED = "heapMemoryUsageCommitted";
		public static final String ROOTS = "roots";
		public static final String PATH = "path";
		public static final String USABLE_SPACE = "usableSpace";
		public static final String USED_SPACE = "usedSpace";
		public static final String FREE_SPACE = "freeSpace";
		public static final String TOTAL_SPACE = "totalSpace";
		public static final String CONFIG_PATH = "configPath";
		public static final String DATA_PATH = "dataPath";
		public static final String TEMP_PATH = "tempPath";
		public static final String SUPPORTED_LANGUAGES = "supportedLanguages";
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
