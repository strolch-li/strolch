package ch.eitchnet.utils.objectfilter;

/**
 * A discrete set of operations associated to some object / state
 * 
 * @author Michael Gatto <michael@gatto.ch>
 */
public enum Operation {
	/**
	 * ADD The operation associated to something is addition.
	 */
	ADD,
	/**
	 * MODIFY The operation associated to something is a modification.
	 */
	MODIFY,
	/**
	 * REMOVE The operation associated to something is removal
	 */
	REMOVE;
}
