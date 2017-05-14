package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

/**
 * The {@link ActivityArchivalPolicy} is called when an {@link Activity} has reached the state {@link State#EXECUTED}
 * and can thus be archived. Here the archivation of the {@link Activity} can be implemented, e.g. removing it, or
 * exporting it to a different system etc.
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ActivityArchivalPolicy extends StrolchPolicy {

	public ActivityArchivalPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void archive(Activity activity) {
		// do nothing
	}

	@Override
	public void undo() {
		// nothing to undo
	}
}
