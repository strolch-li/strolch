package li.strolch.model;

import static ch.eitchnet.utils.helper.StringHelper.NULL;

import java.util.Date;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

public class ModelStatistics {

	public Date startTime;
	public long durationNanos;
	public long nrOfResources;
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

	public long getNrOfElements() {
		return this.nrOfOrders + this.nrOfResources;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName() + " [startTime=");
		builder.append(this.startTime == null ? NULL : ISO8601FormatFactory.getInstance()
				.formatDate(this.startTime));
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