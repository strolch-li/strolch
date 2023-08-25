package li.strolch.search;

import li.strolch.model.Resource;

import java.util.stream.Stream;

public class ResourceSearchResult extends RootElementSearchResult<Resource> {
	public ResourceSearchResult(Stream<Resource> stream) {
		super(stream);
	}
}
