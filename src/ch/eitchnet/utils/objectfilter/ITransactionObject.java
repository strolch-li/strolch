package ch.eitchnet.utils.objectfilter;

/**
 * This interface serves for objects which are required, at some point, to have a unique ID within a transaction.
 * 
 * @author Michael Gatto <michael@gatto.ch>
 */
public interface ITransactionObject {

	/**
	 * UNSET Marker to determine if ids have not been set.
	 * <p>
	 * Beware: this is set to 0 due to transient field in the {@link ITransactionObject} implementations that store the
	 * ID, which are set to zero when de-serialized, and that are not allowed to be serialized.
	 * </p>
	 */
	public static final long UNSET = 0;

	/**
	 * Set the ID of this object. This ID is unique for this object within the transaction.
	 * 
	 * @param id
	 *            The ID to set.
	 */
	public void setTransactionID(long id);

	/**
	 * @return The ID of this object, as set within the transaction. This ID shall guarantee that it is unique within
	 *         this transaction.
	 */
	public long getTransactionID();

	/**
	 * Reset / anul the transaction ID of this object
	 */
	public void resetTransactionID();
}
