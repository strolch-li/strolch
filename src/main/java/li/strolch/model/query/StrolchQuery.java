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
package li.strolch.model.query;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public abstract class StrolchQuery<T extends QueryVisitor> {

	private Navigation navigation;
	protected List<Selection> selections;

	public StrolchQuery(Navigation navigation) {
		this.navigation = navigation;
		this.selections = new ArrayList<>();
	}

	public void addSelection(BooleanSelection<Selection> selection) {
		this.selections.add(selection);
	}

	public void addSelection(StrolchElementSelection selection) {
		this.selections.add(selection);
	}

	public void addSelection(ParameterSelection selection) {
		this.selections.add(selection);
	}

	public void accept(T visitor) {
		this.navigation.accept(visitor);
		for (Selection selection : this.selections) {
			selection.accept(visitor);
		}
	}
}
