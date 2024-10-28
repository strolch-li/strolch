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
package li.strolch.service;

import li.strolch.model.ModelStatistics;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class XmlImportModelResult extends ServiceResult {

	private ModelStatistics statistics;

	public XmlImportModelResult(ModelStatistics statistics) {
		super(ServiceResultState.SUCCESS);
		this.statistics = statistics;
	}

	public XmlImportModelResult() {
		// no arg constructor
	}

	public XmlImportModelResult(ServiceResultState state, String message, Throwable throwable) {
		super(state, message, throwable);
	}

	public XmlImportModelResult(ServiceResultState state, String message) {
		super(state, message);
	}

	public XmlImportModelResult(ServiceResultState state) {
		super(state);
	}

	public ModelStatistics getStatistics() {
		return statistics;
	}
}
