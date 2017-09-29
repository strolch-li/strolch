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
package li.strolch.rest.helper;

import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.QueryData;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RestfulHelper {

	private static final Logger logger = LoggerFactory.getLogger(RestfulHelper.class);

	public static Locale getLocale(HttpHeaders headers) {
		if (headers == null || StringHelper.isEmpty(headers.getHeaderString(HttpHeaders.ACCEPT_LANGUAGE)))
			return null;
		return headers.getAcceptableLanguages().get(0);
	}

	public static Certificate getCert(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		DBC.PRE.assertNotNull("Certificate not found as request attribute!", cert);
		return cert;
	}

	public static <T extends StrolchRootElement> JsonObject toJson(QueryData queryData, long dataSetSize,
			List<T> elements, StrolchElementVisitor<JsonObject> toJsonVisitor) {

		// paging
		Paging<T> paging = Paging.asPage(elements, queryData.getOffset(), queryData.getLimit());

		// get page
		List<T> page = paging.getPage();

		JsonObject root = new JsonObject();
		root.addProperty("msg", "-");
		root.addProperty("limit", paging.getLimit());
		root.addProperty("offset", paging.getOffset());
		root.addProperty("size", paging.getSize());
		root.addProperty("previousOffset", paging.getPreviousOffset());
		root.addProperty("nextOffset", paging.getNextOffset());
		root.addProperty("lastOffset", paging.getLastOffset());

		root.addProperty("dataSetSize", dataSetSize);

		if (StringHelper.isNotEmpty(queryData.getOrderBy()))
			root.addProperty("sortBy", queryData.getOrderBy());
		root.addProperty("descending", queryData.isDescending());

		// add items
		JsonArray data = new JsonArray();
		for (T t : page) {
			JsonObject element = t.accept(toJsonVisitor);
			data.add(element);
		}
		root.add("data", data);
		return root;
	}

	public static <T extends StrolchRootElement> void doOrdering(QueryData queryData, List<T> resources) {
		if (StringHelper.isNotEmpty(queryData.getOrderBy())) {
			if (queryData.getOrderBy().equals(Tags.Json.ID)) {
				resources.sort((r1, r2) -> !queryData.isDescending() ? r1.getId().compareTo(r2.getId())
						: r2.getId().compareTo(r1.getId()));
			} else if (queryData.getOrderBy().equals(Tags.Json.NAME)) {
				resources.sort((r1, r2) -> !queryData.isDescending() ? r1.getName().compareTo(r2.getName())
						: r2.getName().compareTo(r1.getName()));
			} else if (queryData.getOrderBy().equals(Tags.Json.TYPE)) {
				resources.sort((r1, r2) -> !queryData.isDescending() ? r1.getType().compareTo(r2.getType())
						: r2.getType().compareTo(r1.getType()));
			} else {
				logger.warn("Unhandled ordering " + queryData.getOrderBy());
			}
		}
	}
}
