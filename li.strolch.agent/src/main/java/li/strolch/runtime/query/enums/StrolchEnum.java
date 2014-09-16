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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "StrolchEnum")
@XmlType(propOrder = { "name", "locale", "values" })
public class StrolchEnum {

	@XmlAttribute(name = "name")
	private String name;

	@XmlAttribute(name = "locale")
	private String locale;

	private Map<String, EnumValue> values;

	@XmlTransient
	private Locale localeL;

	public StrolchEnum() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param name
	 * @param locale
	 * @param values
	 */
	public StrolchEnum(String name, Locale locale, Map<String, EnumValue> values) {
		this.name = name;
		this.locale = locale.toString();
		this.localeL = locale;
		this.values = values;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the locale as string
	 */
	public String getLocale() {
		return this.locale;
	}

	/**
	 * @param locale
	 *            the locale to set
	 */
	public void setLocale(String locale) {
		this.localeL = new Locale(locale);
		this.locale = locale;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocaleL() {
		return this.localeL;
	}

	/**
	 * @param localeL
	 *            the localeL to set
	 */
	public void setLocaleL(Locale localeL) {
		this.locale = localeL.getLanguage() + StringHelper.UNDERLINE + localeL.getCountry();
		this.localeL = localeL;
	}

	/**
	 * @return the values
	 */
	@XmlElement(name = "values")
	public Collection<EnumValue> getValues() {
		if (this.values == null)
			return null;
		return this.values.values();
	}

	/**
	 * @param values
	 *            the values to set
	 */
	public void setValues(Collection<EnumValue> values) {
		this.values = new HashMap<>(values.size());
		for (EnumValue enumValue : values) {
			this.values.put(enumValue.getId(), enumValue);
		}
	}

	/**
	 * @return the list of {@link EnumValue#getId()}
	 */
	public List<String> getEnumValueIds() {
		return new ArrayList<>(this.values.keySet());
	}

	/**
	 * @return the list of {@link EnumValue#getValue()}
	 */
	public List<String> getEnumValues() {
		List<String> values = new ArrayList<>(this.values.size());
		for (EnumValue enumValue : this.values.values()) {
			values.add(enumValue.getValue());
		}

		return values;
	}

	/**
	 * Returns the actual value for the given id
	 * 
	 * @param id
	 *            the id of the value to return
	 * 
	 * @return the actual value for the given id, or null if it does not exist
	 */
	public String getValue(String id) {
		EnumValue enumValue = this.values.get(id);
		if (enumValue == null)
			return null;
		return enumValue.getValue();
	}
}
