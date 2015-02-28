package li.strolch.model;

import static ch.eitchnet.utils.helper.StringHelper.NULL;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import li.strolch.model.xml.Iso8601DateAdapter;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "ModelStatistics")
@XmlAccessorType(XmlAccessType.NONE)
public class ModelStatistics {

	@XmlAttribute(name = "startTime")
	@XmlJavaTypeAdapter(value = Iso8601DateAdapter.class)
	public Date startTime;

	@XmlAttribute(name = "duractionNanos")
	public long durationNanos;

	@XmlAttribute(name = "nrOfResources")
	public long nrOfResources;

	@XmlAttribute(name = "nrOfOrders")
	public long nrOfOrders;

	/**
	 * @return the nrOfOrders
	 */
	public long getNrOfOrders() {
		return this.nrOfOrders;
	}

	/**
	 * @return the nrOfResources
	 */
	public long getNrOfResources() {
		return this.nrOfResources;
	}

	/**
	 * @return the nrOfResources + nrOfOrders
	 */
	public long getNrOfElements() {
		return this.nrOfOrders + this.nrOfResources;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName() + " [startTime=");
		builder.append(this.startTime == null ? NULL : ISO8601FormatFactory.getInstance().formatDate(this.startTime));
		builder.append(", durationNanos=");
		builder.append(StringHelper.formatNanoDuration(this.durationNanos));
		builder.append(", nrOfResources=");
		builder.append(this.nrOfResources);
		builder.append(", nrOfOrders=");
		builder.append(this.nrOfOrders);
		builder.append("]");
		return builder.toString();
	}
}