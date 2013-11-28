/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.query.inmemory;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class BooleanSelector<T extends StrolchElement> implements Selector<T> {

	protected List<Selector<T>> selectors;

	public BooleanSelector(Selector<T> leftHandSide, Selector<T> rightHandSide) {
		this.selectors = new ArrayList<>();
		this.selectors.add(leftHandSide);
		this.selectors.add(rightHandSide);
	}

	public BooleanSelector(List<Selector<T>> selectors) {
		this.selectors = selectors;
	}

	public abstract boolean select(T element);
}
