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

import java.nio.charset.StandardCharsets;

import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.privilege.base.PrivilegeConstants;

public class StrolchModelConstants {

	public static final String DEFAULT_XML_VERSION = "1.0"; //$NON-NLS-1$

	public static final String DEFAULT_ENCODING = StandardCharsets.UTF_8.name(); //$NON-NLS-1$

	/**
	 * The type to set on {@link StrolchRootElement StrolchRootElements} when defining a template for a type of element
	 */
	public static final String TEMPLATE = "Template"; //$NON-NLS-1$

	public static final String SUFFIX_REF = "-Ref";

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as an
	 * enumeration
	 */
	public static final String INTERPRETATION_ENUMERATION = "Enumeration"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to a {@link Resource}
	 */
	public static final String INTERPRETATION_RESOURCE_REF = Tags.RESOURCE + SUFFIX_REF; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to an {@link Order}
	 */
	public static final String INTERPRETATION_ORDER_REF = Tags.ORDER + SUFFIX_REF; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to an {@link Activity}
	 */
	public static final String INTERPRETATION_ACTIVITY_REF = Tags.ACTIVITY + SUFFIX_REF; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the {@link Parameter} has no defined interpretation
	 */
	public static final String INTERPRETATION_NONE = "None"; //$NON-NLS-1$

	/**
	 * This uom value indicates that the {@link Parameter} has no defined uom
	 */
	public static final String UOM_NONE = "None"; //$NON-NLS-1$

	public static final String INTERNAL = "internal";

	public static final String BAG_RELATIONS = "relations";
	public static final String BAG_PARAMETERS = "parameters";
	public static final String TYPE_PARAMETERS = "Parameters";
	public static final String TYPE_VERSION = "Version";
	public static final String TYPE_MEMORY = "Memory";
	public static final String TYPE_OPERATING_SYSTEM = "OperatingSystem";
	public static final String TYPE_ROOT = "Root";
	public static final String TYPE_RELATIONS = "Relations";
	public static final String TYPE_ENUMERATION = "Enumeration";
	public static final String TYPE_CONFIGURATION = "Configuration";
	public static final String TYPE_OBJECTIVES = "Objectives";
	public static final String TYPE_METRIC = "Metric";

	public static final String RES_CONFIGURATION = "configuration";

	public static final String STATE_VALUES = "values";

	/**
	 * ID of the admin role which has access to all resources
	 */
	public static final String ROLE_STROLCH_ADMIN = PrivilegeConstants.ROLE_STROLCH_ADMIN;

	public static final String PARAM_CLASS_NAME = "className";
	public static final String PARAM_CRON = "cron";
	public static final String PARAM_INITIAL_DELAY = "initialDelay";
	public static final String PARAM_DELAY = "delay";
	public static final String PARAM_START_DATE = "startDate";
	public static final String PARAM_MODE = "mode";
	public static final String PARAM_GROUP = "group";

	public static class PolicyConstants {
		public static final String POLICY_DEFAULT = "Default";

		public static final String TYPE_PRODUCE = "Produce";
		public static final String TYPE_CONSUME = "Consume";
		public static final String TYPE_RESERVE = "Reserve";
		public static final String TYPE_RELEASE = "Release";
		public static final String TYPE_USE = "Use";
		public static final String TYPE_WAIT = "Wait";

		public static final String BAG_OBJECTIVES = "Objectives";

		public static final String PARAM_RESERVED = "reserved";
		public static final String PARAM_QUANTITY = "quantity";
		public static final String PARAM_DURATION = "duration";
		public static final String PARAM_ORDER = "order";
		public static final String PARAM_ACTIVITY = "activity";
	}
}
