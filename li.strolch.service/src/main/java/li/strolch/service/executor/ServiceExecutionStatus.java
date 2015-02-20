package li.strolch.service.executor;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.service.api.ServiceResult;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "ServiceExecutionStatus")
@XmlAccessorType(XmlAccessType.NONE)
public class ServiceExecutionStatus {

	private String serviceName;
	private volatile boolean started;
	private volatile ServiceResult result;

	public ServiceExecutionStatus() {
		// no arg constructor for JAXB
	}

	public ServiceExecutionStatus(String serviceName) {
		this.serviceName = serviceName;
	}

	@XmlAttribute(name = "serviceName")
	public String getServiceName() {
		return serviceName;
	}

	public synchronized ServiceResult getResult() {
		return result;
	}

	public synchronized void setResult(ServiceResult svcResult) {
		this.result = svcResult;
	}

	@XmlAttribute(name = "resultMessage")
	public String getResultMessage() {
		if (this.result == null)
			return StringHelper.DASH;
		if (this.result.getMessage() == null)
			return StringHelper.DASH;
		return this.result.getMessage();
	}

	@XmlAttribute(name = "resultState")
	public String getResultState() {
		if (this.result == null)
			return StringHelper.DASH;
		return this.result.getState().name();
	}

	@XmlAttribute(name = "done")
	public boolean isDone() {
		return this.result != null;
	}

	@XmlAttribute(name = "started")
	public synchronized boolean isStarted() {
		return started;
	}

	public synchronized void started() {
		this.started = true;
	}
}