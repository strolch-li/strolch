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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.HttpHeaders;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.QueryData;
import li.strolch.search.RootElementSearchResult;
import li.strolch.search.SearchResult;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Locale;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RestfulHelper {

	private static final Logger logger = LoggerFactory.getLogger(RestfulHelper.class);

	public static Locale getLocale(HttpHeaders headers) {
		if (headers == null || StringHelper.isEmpty(headers.getHeaderString(HttpHeaders.ACCEPT_LANGUAGE)))
			return null;
		return headers.getAcceptableLanguages().getFirst();
	}

	public static Certificate getCert(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		DBC.PRE.assertNotNull("Certificate not found as request attribute!", cert);
		return cert;
	}

	public static <T extends StrolchRootElement> JsonObject toJson(QueryData queryData, long dataSetSize,
			RootElementSearchResult<T> result, StrolchRootElementVisitor<JsonObject> toJsonVisitor) {

		Paging<T> paging = result.toPaging(queryData.getOffset(), queryData.getLimit());
		Function<T, JsonObject> toJsonFunc = t -> t.accept(toJsonVisitor);
		return toJson(queryData, dataSetSize, paging, toJsonFunc);
	}

	public static <T> JsonObject toJson(QueryData queryData, long dataSetSize, SearchResult<T> result,
			Function<T, JsonObject> toJsonVisitor) {

		Paging<T> paging = result.toPaging(queryData.getOffset(), queryData.getLimit());
		return toJson(queryData, dataSetSize, paging, toJsonVisitor);
	}

	public static <T> JsonObject toJson(QueryData queryData, long dataSetSize, Stream<T> stream,
			Function<T, JsonObject> toJsonVisitor) {

		Paging<T> paging = Paging.asPage(stream.collect(toList()), queryData.getOffset(), queryData.getLimit());
		return toJson(queryData, dataSetSize, paging, toJsonVisitor);
	}

	public static <T> JsonObject toJson(QueryData queryData, long dataSetSize, Paging<T> paging,
			Function<T, JsonObject> toJsonVisitor) {

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
			data.add(toJsonVisitor.apply(t));
		}
		root.add("data", data);
		return root;
	}

	public static String getRemoteIp(HttpServletRequest request) {
		if (request == null) {
			logger.error("HttpServletRequest NOT AVAILABLE! Probably running in TEST!");
			return "(null)";
		}

		String remoteHost = request.getRemoteHost();
		String remoteAddr = request.getRemoteAddr();

		StringBuilder sb = new StringBuilder();
		if (remoteHost.equals(remoteAddr))
			sb.append(remoteAddr);
		else {
			sb.append(remoteHost).append(": (").append(remoteAddr).append(")");
		}

		String xForwardedFor = request.getHeader("X-Forwarded-For");
		if (isNotEmpty(xForwardedFor))
			sb.append(" (fwd)=> ").append(xForwardedFor);

		return sb.toString();
	}
}
