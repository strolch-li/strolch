package li.strolch.model;

import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.policy.PolicyDefs;

/**
 * A {@link PolicyContainer} has a reference to {@link PolicyDefs} on which Policy configurations are stored for the
 * given object
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PolicyContainer {

	/**
	 * Returns the reference to the {@link PolicyDefs}
	 * 
	 * @return the reference to the {@link PolicyDefs}
	 * 
	 * @throws StrolchPolicyException
	 *             if no {@link PolicyDefs} are available
	 */
	public PolicyDefs getPolicyDefs() throws StrolchPolicyException;

	/**
	 * @return true if this container has {@link PolicyDefs}, false if not
	 */
	public boolean hasPolicyDefs();

	/**
	 * Set the reference to the {@link PolicyDefs}
	 * 
	 * @param policyDefs
	 *            the {@link PolicyDefs} to set
	 */
	public void setPolicyDefs(PolicyDefs policyDefs);
}
