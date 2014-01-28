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
package li.strolch.runtime.query.enums;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultEnumHandler extends StrolchComponent implements EnumHandler {

	private static final String PROP_REALM = "realm";

	private String realm;
	private Map<String, Locator> enumLocators;

	/**
	 * @param container
	 * @param componentName
	 */
	public DefaultEnumHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		this.enumLocators = new HashMap<>();
		this.realm = configuration.getString(PROP_REALM, StrolchConstants.DEFAULT_REALM);

		Set<String> enumNames = configuration.getPropertyKeys();
		for (String enumName : enumNames) {
			if (enumName.equals(PROP_REALM)) {
				continue;
			}

			String enumLocatorS = configuration.getString(enumName, null);
			Locator enumLocator = Locator.valueOf(enumLocatorS);
			logger.info(MessageFormat.format("Registered enum {0} at {1}", enumName, enumLocator));
			enumLocators.put(enumName, enumLocator);
		}

		super.initialize(configuration);
	}

	@Override
	public StrolchEnum getEnum(String name, Locale locale) {

		DBC.PRE.assertNotEmpty("Enum name must be given!", name);
		DBC.PRE.assertNotNull("Locale must be given!", locale);

		Locator enumLocator = this.enumLocators.get(name);
		if (enumLocator == null)
			throw new StrolchException(MessageFormat.format("No enumeration is configured for the name {0}", name));

		try (StrolchTransaction tx = getContainer().getRealm(realm).openTx()) {
			Resource enumeration = tx.findElement(enumLocator);
			ParameterBag enumValuesByLanguage = findParameterBagByLanguage(enumeration, locale);

			Set<String> parameterKeySet = enumValuesByLanguage.getParameterKeySet();
			List<EnumValue> values = new ArrayList<>(parameterKeySet.size());
			for (String paramKey : parameterKeySet) {
				StringParameter enumParam = enumValuesByLanguage.getParameter(paramKey);
				values.add(new EnumValue(paramKey, enumParam.getValue()));
			}

			StrolchEnum strolchEnum = new StrolchEnum(name, locale, values);
			return strolchEnum;
		}
	}

	/**
	 * @param enumeration
	 * @param locale
	 * @return
	 */
	private ParameterBag findParameterBagByLanguage(Resource enumeration, Locale locale) {

		String localeS = locale.getLanguage() + "_" + locale.getCountry() + "_" + locale.getVariant();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		localeS = locale.getLanguage() + "_" + locale.getCountry();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		localeS = locale.getLanguage();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		String msg = "No enumeration exists for language {0} on enumeration {1}";
		msg = MessageFormat.format(msg, locale, enumeration.getLocator());
		throw new StrolchException(msg);
	}
}
