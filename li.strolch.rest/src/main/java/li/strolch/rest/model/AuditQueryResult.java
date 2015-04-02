package li.strolch.rest.model;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.audit.Audit;
import ch.eitchnet.utils.helper.StringHelper;

@XmlRootElement(name = "AuditQueryResult")
@XmlAccessorType(XmlAccessType.NONE)
public class AuditQueryResult {

	@XmlAttribute(name = "msg")
	private String msg;

	@XmlElement(name = "audits")
	private List<Audit> audits;

	public AuditQueryResult() {
		this.msg = StringHelper.DASH;
	}

	public AuditQueryResult(List<Audit> audits) {
		this.msg = StringHelper.DASH;
		this.audits = audits;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public List<Audit> getAudits() {
		return audits;
	}

	public void setAudits(List<Audit> audits) {
		this.audits = audits;
	}
}
