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
