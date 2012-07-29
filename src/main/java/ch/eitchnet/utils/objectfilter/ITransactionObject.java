/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
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
