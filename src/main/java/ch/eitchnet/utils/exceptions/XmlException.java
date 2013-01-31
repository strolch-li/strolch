/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the ch.eitchnet.utils.
 *
 *  ch.eitchnet.utils is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  ch.eitchnet.utils is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ch.eitchnet.utils.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.exceptions;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	/**
	 * @param message
	 */
	public XmlException(String message) {
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public XmlException(String message, Throwable cause) {
		super(message, cause);
	}
}
