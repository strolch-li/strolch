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
package li.strolch.runtime.query.visitor;

import li.strolch.runtime.query.IdSelection;
import li.strolch.runtime.query.NameSelection;
import li.strolch.runtime.query.OrderTypeNavigation;
import li.strolch.runtime.query.QueryVisitor;
import li.strolch.runtime.query.ResourceTypeNavigation;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface StrolchElementVisitor extends QueryVisitor {

	public void visit(IdSelection selection);

	public void visit(NameSelection selection);

	public void visit(OrderTypeNavigation navigation);

	public void visit(ResourceTypeNavigation navigation);
}
