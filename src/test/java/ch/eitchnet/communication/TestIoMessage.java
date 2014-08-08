package ch.eitchnet.communication;

import java.util.List;

public class TestIoMessage extends IoMessage {

	private List<String> contents;

	public TestIoMessage(String id, CommandKey key) {
		super(id, key);
	}

	public TestIoMessage(String id, CommandKey key, List<String> contents) {
		super(id, key);
		this.contents = contents;
	}

	public List<String> getContents() {
		return this.contents;
	}

	public void setContents(List<String> contents) {
		this.contents = contents;
	}
}
