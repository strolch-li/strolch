/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.policy;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.policy.JavaPolicyDef;
import li.strolch.model.policy.KeyPolicyDef;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;
import li.strolch.policy.StrolchPolicyFileParser.PolicyType;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;

/**
 * <p>
 * This is the default Strolch {@link PolicyHandler} which implements {@link PolicyDefVisitor}. This
 * {@link PolicyHandler} parses a policy configuration file to handle the look-up of {@link KeyPolicyDef}
 * </p>
 * <p>
 * This {@link StrolchComponent} uses two configuration properties:
 * <ul>
 * <li>{@link #PROP_READ_POLICY_FILE} - Default is false. If false then no configuration file is read. Useful if all
 * policy definitions are {@link JavaPolicyDef}</li>
 * <li>{@link #PROP_POLICY_CONFIG} - Default is {@link #DEF_STROLCH_POLICIES_XML}. If {@link #PROP_READ_POLICY_FILE} is
 * true, then this property is used to determine which configuration file to parse for the policy key mappings for
 * {@link KeyPolicyDef}</li>
 * </ul>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPolicyHandler extends StrolchComponent implements PolicyHandler, PolicyDefVisitor {

	private static final String PROP_POLICY_CONFIG = "policyConfig";
	private static final String PROP_READ_POLICY_FILE = "readPolicyFile";
	private static final String DEF_STROLCH_POLICIES_XML = "StrolchPolicies.xml";

	private MapOfMaps<String, String, Class<? extends StrolchPolicy>> classByTypeMap;

	public DefaultPolicyHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		reloadPolicies(configuration);
		super.initialize(configuration);
	}

	public Set<String> getPolicyTypes() {
		return new HashSet<>(this.classByTypeMap.keySet());
	}

	public Set<String> getPolicyKeysByType(String type) {
		Map<String, Class<? extends StrolchPolicy>> byType = this.classByTypeMap.getMap(type);
		if (byType == null)
			return Collections.emptySet();
		return new HashSet<>(byType.keySet());
	}

	public Set<String> getPolicyKeysByType(Class<?> clazz) {
		Map<String, Class<? extends StrolchPolicy>> byType = this.classByTypeMap.getMap(clazz.getSimpleName());
		if (byType == null)
			return Collections.emptySet();
		return new HashSet<>(byType.keySet());
	}

	@Override
	public <T extends StrolchPolicy> T getPolicy(PolicyDef policyDef, StrolchTransaction tx) {
		return getPolicy(policyDef, null, tx);
	}

	@Override
	public <T extends StrolchPolicy> T getPolicy(PolicyDef policyDef, PolicyDef defaultDef, StrolchTransaction tx) {
		DBC.PRE.assertNotNull("policyDef must not be null!", policyDef);
		DBC.PRE.assertNotNull("tx must not be null!", tx);
		try {

			if (defaultDef != null && !isPolicyDefAvailable(policyDef))
				policyDef = defaultDef;

			Class<T> clazz = policyDef.accept(this);
			if (clazz.isInterface() || Modifier.isAbstract(clazz.getModifiers()))
				throw new IllegalArgumentException(
						"Class " + clazz.getName() + " can not be instantiated as it is not a concrete class!");

			@SuppressWarnings("unchecked")
			Constructor<T> constructor = (Constructor<T>) getConstructorForPolicy(clazz);
			if (constructor.getParameterCount() == 1)
				return constructor.newInstance(tx);
			return constructor.newInstance(tx);

		} catch (StrolchPolicyException e) {
			throw e;
		} catch (Exception e) {
			throw new StrolchPolicyException(
					MessageFormat.format("Failed to instantiate policy {0} due to {1}", policyDef, e.getMessage()), e);
		}
	}

	@Override
	public boolean isPolicyDefAvailable(PolicyDef policyDef) {
		DBC.PRE.assertNotNull("policyDef must not be null!", policyDef);

		if (policyDef instanceof KeyPolicyDef)
			return this.classByTypeMap.containsElement(policyDef.getType(), policyDef.getValue());

		if (policyDef instanceof JavaPolicyDef)
			return true;

		throw new IllegalArgumentException("Unhandled PolicyDev instance " + policyDef.getClass());
	}

	@Override
	public <T> Class<T> visit(JavaPolicyDef policyDef) throws ClassNotFoundException {
		String value = policyDef.getValue();
		@SuppressWarnings("unchecked")
		Class<T> clazz = (Class<T>) Class.forName(value);
		return clazz;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> Class<T> visit(KeyPolicyDef policyDef) {
		Class<?> clazz = this.classByTypeMap.getElement(policyDef.getType(), policyDef.getValue());
		if (clazz == null)
			throw new StrolchPolicyException(MessageFormat.format("No policy is configured for {0}", policyDef));
		return (Class<T>) clazz;
	}

	@Override
	public void reloadPolicies() {
		reloadPolicies(getConfiguration());
	}

	private void reloadPolicies(ComponentConfiguration configuration) {
		if (configuration.getBoolean(PROP_READ_POLICY_FILE, Boolean.TRUE)) {
			File policyFile = configuration.getConfigFile(PROP_POLICY_CONFIG, DEF_STROLCH_POLICIES_XML,
					configuration.getRuntimeConfiguration());
			parsePolicyFile(policyFile);
		} else {
			logger.warn("Not loading Policy configuration file, as disabled by config");
		}
	}

	@SuppressWarnings("unchecked")
	private void parsePolicyFile(File policyFile) {

		// first we parse the file
		StrolchPolicyFileParser xmlHandler = new StrolchPolicyFileParser();
		XmlHelper.parseDocument(policyFile, xmlHandler);
		PolicyModel policyModel = xmlHandler.getPolicyModel();

		// then we iterate the parsed model, validating that we can:
		// - access the API class
		// - instantiate the class
		// - assign the class to the API
		Map<String, PolicyType> policyTypes = policyModel.getPolicyTypes();
		this.classByTypeMap = new MapOfMaps<>();
		for (PolicyType policyType : policyTypes.values()) {

			String type = policyType.getType();
			String api = policyType.getApi();

			try {

				// get the API class
				Class<?> apiClass = Class.forName(api);

				// now iterate all configured policies for this API
				Map<String, String> policyByKeyMap = policyType.getPolicyByKeyMap();
				for (String key : policyByKeyMap.keySet()) {
					String className = policyByKeyMap.get(key);

					try {

						// get the class
						Class<?> implClass = Class.forName(className);

						// assert API is a Policy
						if (!StrolchPolicy.class.isAssignableFrom(implClass)) {
							throw new StrolchPolicyException(
									"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
											+ " Key=" + key + " as " + className + " is not a "
											+ StrolchPolicy.class.getName());
						}

						// assert is assignable to API class
						if (!apiClass.isAssignableFrom(implClass)) {
							throw new StrolchPolicyException(
									"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
											+ " Key=" + key + " as " + className + " is not assignable from " + api);
						}

						// and assert is not abstract
						if (Modifier.isAbstract(implClass.getModifiers()) || Modifier.isInterface(
								implClass.getModifiers()))
							throw new IllegalStateException(
									"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
											+ " Key=" + key + " as " + className + " is abstract or an interface!");

						Constructor<?> constructor;
						try {
							constructor = getConstructorForPolicy(implClass);
						} catch (NoSuchMethodException e) {
							throw new IllegalStateException(
									"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
											+ " Key=" + key
											+ " as constructor (StrolchTransaction) or (ComponentContainer, StrolchTransaction) does not exist!");
						}

						if (Modifier.isAbstract(constructor.getModifiers()) || Modifier.isInterface(
								constructor.getModifiers()))
							throw new IllegalStateException(
									"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
											+ " Key=" + key + " as constructor is abstract or an interface!");

						// store the implementation class
						logger.info("Loaded Policy " + type + " / " + key + " / " + className);
						this.classByTypeMap.addElement(type, key, (Class<? extends StrolchPolicy>) implClass);

					} catch (ClassNotFoundException e) {
						throw new StrolchPolicyException(
								"Invalid " + StrolchPolicyFileParser.POLICY + " configuration for Type=" + type
										+ " Key=" + key + " due to " + e.getMessage(), e);
					}
				}
			} catch (ClassNotFoundException e) {
				throw new StrolchPolicyException(
						"Invalid " + StrolchPolicyFileParser.POLICY_TYPE + " configuration for Type=" + type
								+ " due to " + e.getMessage(), e);
			}
		}
	}

	private Constructor<?> getConstructorForPolicy(Class<?> implClass) throws NoSuchMethodException {
		Constructor<?> constructor = null;
		Constructor<?>[] constructors = implClass.getConstructors();
		for (Constructor<?> c : constructors) {
			Class<?>[] parameterTypes = c.getParameterTypes();
			if (parameterTypes.length == 1 && parameterTypes[0] == StrolchTransaction.class) {
				constructor = c;
				break;
			}

			if (parameterTypes.length == 2 && parameterTypes[0] == ComponentContainer.class
					&& parameterTypes[1] == StrolchTransaction.class) {
				constructor = c;
				break;
			}
		}

		if (constructor == null)
			throw new NoSuchMethodException();

		return constructor;
	}
}
