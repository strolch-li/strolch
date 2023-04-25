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
package li.strolch.rest;

import jakarta.ws.rs.core.MediaType;

import li.strolch.model.Tags;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchRestfulConstants {

	public static final String STROLCH_CERTIFICATE = "strolch.certificate";
	public static final String STROLCH_REQUEST_SOURCE= "strolch.requestSource";
	public static final String STROLCH_AUTHORIZATION = "strolch.authorization";
	public static final String STROLCH_AUTHORIZATION_EXPIRATION_DATE = "strolch.authorization.expirationDate";

	public static final String MSG = "msg";
	public static final String I18N = "i18n";
	public static final String EXCEPTION_MSG = "exceptionMsg";
	public static final String DATA = Tags.Json.DATA;
	public static final String LAST_OFFSET = "lastOffset";
	public static final String NEXT_OFFSET = "nextOffset";
	public static final String PREVIOUS_OFFSET = "previousOffset";
	public static final String DATA_SET_SIZE = "dataSetSize";
	public static final String SIZE = "size";
	public static final String OFFSET = "offset";
	public static final String LIMIT = "limit";

	public static final String PARAM_DATE_RANGE = "dateRange";
	public static final String PARAM_DATE_RANGE_SEL = "dateRangeSel";
	public static final String PARAM_FACETS = "facets";
	public static final String PARAM_FROM = "from";
	public static final String PARAM_TO = "to";
	public static final String PARAM_FILTER = "filter";
	public static final String PARAM_QUERY = "query";

	public static final MediaType TEXT_CSV_TYPE = new MediaType("text", "csv");
	public static final String TEXT_CSV = "text/csv";
}
