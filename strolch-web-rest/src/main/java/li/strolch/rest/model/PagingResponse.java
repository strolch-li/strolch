/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.rest.model;

import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.utils.collections.Paging;

import java.util.List;

import static li.strolch.rest.StrolchRestfulConstants.*;

@Schema(description = "Represents paging data")
public class PagingResponse<T> {

	@Schema(description = "Total number of available records.")
	private final long dataSetSize;

	@Schema(description = "Maximum number of records returned per request.")
	private final int limit;

	@Schema(description = "Starting point for fetching records.")
	private final int offset;

	@Schema(description = "Number of records in the current page.")
	private final int size;

	@Schema(description = "Offset for the next set of records.")
	private final int nextOffset;

	@Schema(description = "Offset for the previous set of records.")
	private final int previousOffset;

	@Schema(description = "Offset for the last set of records.")
	private final int lastOffset;

	@Schema(description = "The page of data", type = "array")
	private final List<T> data;

	public PagingResponse(Paging<T> paging) {
		this.dataSetSize = paging.getDataSetSize();
		this.limit = paging.getLimit();
		this.offset = paging.getOffset();
		this.size = paging.getSize();
		this.nextOffset = paging.getNextOffset();
		this.previousOffset = paging.getPreviousOffset();
		this.lastOffset = paging.getLastOffset();

		this.data = paging.getPage();
	}

	public long getDataSetSize() {
		return this.dataSetSize;
	}

	public int getLimit() {
		return this.limit;
	}

	public int getOffset() {
		return this.offset;
	}

	public int getSize() {
		return this.size;
	}

	public int getNextOffset() {
		return this.nextOffset;
	}

	public int getPreviousOffset() {
		return this.previousOffset;
	}

	public int getLastOffset() {
		return this.lastOffset;
	}

	public List<T> getData() {
		return this.data;
	}

	public void addPagingInfo(JsonObject response) {
		response.addProperty(DATA_SET_SIZE, getDataSetSize());
		response.addProperty(LIMIT, getLimit());
		response.addProperty(OFFSET, getOffset());
		response.addProperty(SIZE, getSize());
		response.addProperty(PREVIOUS_OFFSET, getPreviousOffset());
		response.addProperty(NEXT_OFFSET, getNextOffset());
		response.addProperty(LAST_OFFSET, getLastOffset());
	}
}
