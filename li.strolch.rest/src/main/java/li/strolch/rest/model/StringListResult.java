package li.strolch.rest.model;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.utils.helper.StringHelper;

@XmlRootElement(name = "StringListResult")
@XmlAccessorType(XmlAccessType.NONE)
public class StringListResult {

	@XmlAttribute(name = "msg")
	private String msg;

	@XmlElement(name = "data")
	private List<String> data;

	public StringListResult() {
		this.msg = StringHelper.DASH;
	}

	public StringListResult(List<String> data) {
		this.msg = StringHelper.DASH;
		this.data = data;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public List<String> getData() {
		return data;
	}

	public void setData(List<String> data) {
		this.data = data;
	}
}
