package li.strolch.model.xml;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

public class Iso8601DateAdapter extends XmlAdapter<String, Date> {

	@Override
	public Date unmarshal(String value) throws Exception {
		if (StringHelper.isEmpty(value))
			return null;
		return ISO8601FormatFactory.getInstance().getDateFormat().parse(value);
	}

	@Override
	public String marshal(Date value) throws Exception {
		if (value == null)
			return StringHelper.EMPTY;
		return ISO8601FormatFactory.getInstance().formatDate(value);
	}
}