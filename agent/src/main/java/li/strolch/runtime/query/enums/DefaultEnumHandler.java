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
import static li.strolch.utils.helper.StringHelper.DASH;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import java.text.MessageFormat;
import java.util.*;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
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
		return getEnum(certificate, name, locale, false);
	}

	@Override
	public StrolchEnum getEnum(Certificate certificate, String name, Locale locale, boolean withoutHidden) {
		DBC.PRE.assertNotEmpty("Enum name must be given!", name);
		DBC.PRE.assertNotNull("Locale must be given!", locale);

		try (StrolchTransaction tx = openTx(certificate, true)) {
			return getEnum(tx, name, locale, withoutHidden);
		}
	}

	@Override
	public StrolchEnum getEnum(StrolchTransaction tx, String name) {
		return getEnum(tx, name, tx.getLocale(), false);
	}

	@Override
	public StrolchEnum getEnum(StrolchTransaction tx, String name, boolean withoutHidden) {
		return getEnum(tx, name, tx.getLocale(), withoutHidden);
	}

	private StrolchEnum getEnum(StrolchTransaction tx, String name, Locale locale, boolean withoutHidden) {
		Resource enumeration = tx.getResourceBy(TYPE_ENUMERATION, name, true);
		ParameterBag enumValuesByLanguage = findParameterBagByLanguage(enumeration, locale);

		List<Parameter<?>> parameters = enumValuesByLanguage.getParameters();
		parameters.sort(Comparator.comparing(Parameter::getIndex));
		Map<String, String> values = new LinkedHashMap<>(parameters.size());
		for (Parameter<?> param : parameters) {
			if (withoutHidden && param.isHidden())
				continue;
			StringParameter enumParam = (StringParameter) param;
			values.put(enumParam.getId(), enumParam.getValue());
		}

		return new StrolchEnum(name, locale, values);
	}

	private ParameterBag findParameterBagByLanguage(Resource enumeration, Locale locale) {

		if (isNotEmpty(locale.getVariant())) {
			String localeS = locale.getLanguage() + DASH + locale.getCountry() + DASH + locale.getVariant();
			if (enumeration.hasParameterBag(localeS))
				return enumeration.getParameterBag(localeS);
		}

		if (isNotEmpty(locale.getCountry())) {
			String localeS = locale.getLanguage() + DASH + locale.getCountry();
			if (enumeration.hasParameterBag(localeS))
				return enumeration.getParameterBag(localeS);
		}

		String localeS = locale.getLanguage();
		if (enumeration.hasParameterBag(localeS))
			return enumeration.getParameterBag(localeS);

		String msg = "No enumeration exists for language {0} on enumeration {1}";
		msg = MessageFormat.format(msg, locale, enumeration.getLocator());
		throw new StrolchException(msg);
	}
}
