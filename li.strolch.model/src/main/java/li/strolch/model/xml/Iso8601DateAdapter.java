package li.strolch.model.xml;

import java.util.Calendar;
import java.util.Date;

import javax.xml.bind.DatatypeConverter;
import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

public class Iso8601DateAdapter extends XmlAdapter<String, Date> {

	private static final Logger logger = LoggerFactory.getLogger(Iso8601DateAdapter.class);

	@Override
	public Date unmarshal(String value) throws Exception {
		if (StringHelper.isEmpty(value))
			return null;
		try {
			return DatatypeConverter.parseDateTime(value).getTime();
		} catch (Exception e) {
			IllegalArgumentException ex = new IllegalArgumentException("Failed to parse value: " + value, e);
			logger.info(ex.getMessage(), ex);
			throw ex;
		}
	}

	@Override
	public String marshal(Date value) throws Exception {
		if (value == null)
			return null;
		try {
			Calendar cal = Calendar.getInstance();
			cal.setTime(value);
			String valueS = DatatypeConverter.printDateTime(cal);
			return valueS;
		} catch (Exception e) {
			IllegalArgumentException ex = new IllegalArgumentException("Failed to format date: " + value, e);
			logger.info(ex.getMessage(), ex);
			throw ex;
		}
	}
}