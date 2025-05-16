/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.performance;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class PerformanceTestResult extends ServiceResult {
	private long nrOfTxs;

	public PerformanceTestResult(ServiceResultState state) {
		super(state);
	}

	public PerformanceTestResult(long nrOfTxs) {
		super(ServiceResultState.SUCCESS);
		this.nrOfTxs = nrOfTxs;
	}

	public long getNrOfTxs() {
		return this.nrOfTxs;
	}
}
