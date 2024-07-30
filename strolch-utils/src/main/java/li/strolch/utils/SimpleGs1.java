package li.strolch.utils;

import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class SimpleGs1 {

	private final String gtin;
	private final ZonedDateTime expirationDate;
	private final String batchNo;

	private SimpleGs1(String gs1) {
		if (gs1.length() < 32 || gs1.length() > 46)
			throw new IllegalArgumentException("Can not parse GS1 " + gs1);
		try {
			this.gtin = gs1.substring(2, 14);
			this.expirationDate = LocalDate.parse(gs1.substring(18, 24), DateTimeFormatter.ofPattern("yyMMdd"))
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
		return "SimpleGs1{" + "gtin='" + gtin + '\'' + ", expirationDate=" + expirationDate + ", batchNo='" + batchNo
				+ '\'' + '}';
	}
}
