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

import static li.strolch.model.StrolchModelConstants.TYPE_ENUMERATION;
import static li.strolch.utils.helper.StringHelper.UNDERLINE;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultEnumHandler extends StrolchComponent implements EnumHandler {

	public DefaultEnumHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchEnum getEnum(Certificate certificate, String name, Locale locale) {

		DBC.PRE.assertNotEmpty("Enum name must be given!", name); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Locale must be given!", locale); //$NON-NLS-1$

		try (StrolchTransaction tx = openTx(certificate, true)) {
			Resource enumeration = tx.getResourceBy(TYPE_ENUMERATION, name, true);
			ParameterBag enumValuesByLanguage = findParameterBagByLanguage(enumeration, locale);

			Set<String> parameterKeySet = enumValuesByLanguage.getParameterKeySet();
			Map<String, String> values = new HashMap<>(parameterKeySet.size());
			for (String paramKey : parameterKeySet) {
				StringParameter enumParam = enumValuesByLanguage.getParameter(paramKey);
				values.put(paramKey, enumParam.getValue());
			}

			return new StrolchEnum(name, locale, values);
		}
	}

	private ParameterBag findParameterBagByLanguage(Resource enumeration, Locale locale) {

		String localeS = locale.getLanguage() + UNDERLINE + locale.getCountry() + UNDERLINE + locale.getVariant();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		localeS = locale.getLanguage() + UNDERLINE + locale.getCountry();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		localeS = locale.getLanguage();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		String msg = "No enumeration exists for language {0} on enumeration {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, locale, enumeration.getLocator());
		throw new StrolchException(msg);
	}
}
