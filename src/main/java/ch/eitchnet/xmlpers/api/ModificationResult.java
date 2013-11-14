package ch.eitchnet.xmlpers.api;

import java.util.ArrayList;
import java.util.List;

public class ModificationResult {

	private final String key;
	private final List<?> created;
	private final List<?> updated;
	private final List<?> deleted;

	public ModificationResult(String key) {
		this.key = key;
		this.created = new ArrayList<>();
		this.updated = new ArrayList<>();
		this.deleted = new ArrayList<>();
	}

	public ModificationResult(String key, List<?> created, List<?> updated, List<?> deleted) {
		this.key = key;
		this.created = created;
		this.updated = updated;
		this.deleted = deleted;
	}

	public String getKey() {
		return this.key;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getCreated() {
		return (List<T>) this.created;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getUpdated() {
		return (List<T>) this.updated;
	}

	@SuppressWarnings("unchecked")
	public <T> List<T> getDeleted() {
		return (List<T>) this.deleted;
	}
}
