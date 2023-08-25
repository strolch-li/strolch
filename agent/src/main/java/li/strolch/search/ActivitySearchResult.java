package li.strolch.search;

import li.strolch.model.activity.Activity;

import java.util.stream.Stream;

public class ActivitySearchResult extends RootElementSearchResult<Activity> {
	public ActivitySearchResult(Stream<Activity> stream) {
		super(stream);
	}
}
