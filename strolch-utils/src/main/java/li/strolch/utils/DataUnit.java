package li.strolch.utils;

public enum DataUnit {
	Bytes("bytes", 1),
	KiloBytes("KB", Bytes.bytesFactor * 1000),
	MegaBytes("MB", KiloBytes.bytesFactor * 1000),
	GigaBytes("GB", MegaBytes.bytesFactor * 1000),
	TeraBytes("TB", GigaBytes.bytesFactor * 1000),
	PetaBytes("PB", TeraBytes.bytesFactor * 1000);

	private final String uom;
	private final long bytesFactor;

	DataUnit(String uom, long bytesFactor) {
		this.uom = uom;
		this.bytesFactor = bytesFactor;
	}

	public String getInterpretation() {
		return "DataUnit";
	}

	public String getUom() {
		return this.uom;
	}

	public long getBytesFactor() {
		return this.bytesFactor;
	}

	public double convertToBytes(long value) {
		return value * (double) this.bytesFactor;
	}

	public long roundBytesToUnit(long bytes) {
		return bytes / this.bytesFactor;
	}

	public String humanizeBytesValue(long value) {
		double bytes = convertToBytes(value);

		if (bytes < KiloBytes.bytesFactor)
			return String.format("%f bytes", bytes);

		if (bytes < MegaBytes.bytesFactor)
			return String.format("%.1f KB", (bytes / (double) KiloBytes.bytesFactor));

		if (bytes < GigaBytes.bytesFactor)
			return String.format("%.1f MB", (bytes / (double) MegaBytes.bytesFactor));

		if (bytes < TeraBytes.bytesFactor)
			return String.format("%.1f GB", (bytes / (double) GigaBytes.bytesFactor));

		if (bytes < PetaBytes.bytesFactor)
			return String.format("%.1f TB", (bytes / (double) GigaBytes.bytesFactor));

		return String.format("%.1f PB", (bytes / (double) PetaBytes.bytesFactor));
	}
}
