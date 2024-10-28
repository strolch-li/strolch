/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;

public class SimpleGs1 {

	private final String gtin;
	private final ZonedDateTime expirationDate;
	private final String batchNo;

	private SimpleGs1(String gs1) {
		if (gs1.length() < 32 || gs1.length() > 46)
			throw new IllegalArgumentException("Can not parse GS1 " + gs1);
		try {
			this.gtin = gs1.substring(2, 14);
			this.expirationDate = LocalDate
					.parse(gs1.substring(18, 24), DateTimeFormatter.ofPattern("yyMMdd"))
					.atStartOfDay(ZoneId.systemDefault());
			this.batchNo = gs1.substring(26);
		} catch (Exception e) {
			throw new IllegalArgumentException("Can not parse GS1 " + gs1 + ": " + formatException(getRootCause(e)));
		}
	}

	public String getGtin() {
		return this.gtin;
	}

	public ZonedDateTime getExpirationDate() {
		return this.expirationDate;
	}

	public String getBatchNo() {
		return this.batchNo;
	}

	public static SimpleGs1 valueOf(String gs1) {
		return new SimpleGs1(gs1);
	}

	@Override
	public String toString() {
		return "SimpleGs1{"
				+ "gtin='"
				+ gtin
				+ '\''
				+ ", expirationDate="
				+ expirationDate
				+ ", batchNo='"
				+ batchNo
				+ '\''
				+ '}';
	}
}
