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

import java.util.Locale;
import java.util.Map;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchEnum {

	private String name;
	private String locale;
	private Map<String, String> values;

	private Locale localeL;

	public StrolchEnum(String name, Locale locale, Map<String, String> values) {
		this.name = name;
		this.locale = locale.toString();
		this.localeL = locale;
		this.values = values;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLocale() {
		return this.locale;
	}

	public void setLocale(String locale) {
		this.localeL = new Locale(locale);
		this.locale = locale;
	}

	public Locale getLocaleL() {
		return this.localeL;
	}

	public void setLocaleL(Locale localeL) {
		this.locale = localeL.getLanguage() + StringHelper.UNDERLINE + localeL.getCountry();
		this.localeL = localeL;
	}

	public String getValue(String id) {
		return this.values.get(id);
	}

	public int size() {
		return this.values.size();
	}

	public Set<String> keySet() {
		return this.values.keySet();
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("name", this.name);
		jsonObject.addProperty("locale", this.locale);

		JsonArray valuesJ = new JsonArray();
		for (Map.Entry<String, String> entry : this.values.entrySet()) {
			JsonObject valueJ = new JsonObject();
			valueJ.addProperty(Tags.Json.KEY, entry.getKey());
			valueJ.addProperty(Tags.Json.VALUE, entry.getValue());
			valuesJ.add(valueJ);
		}
		jsonObject.add("values", valuesJ);

		return jsonObject;
	}
}
