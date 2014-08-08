package ch.eitchnet.communication.file;

public enum FileEndpointMode {
	READ(true, false), WRITE(false, true), READ_WRITE(true, true);
	private boolean read;
	private boolean write;

	private FileEndpointMode(boolean read, boolean write) {
		this.read = read;
		this.write = write;
	}

	public boolean isRead() {
		return this.read;
	}

	public boolean isWrite() {
		return this.write;
	}
}