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
package li.strolch.persistence.impl.model;

import li.strolch.model.Resource;
import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.api.ParserFactory;
import ch.eitchnet.xmlpers.api.SaxParser;

public class ResourceParserFactory implements ParserFactory<Resource> {

	@Override
	public DomParser<Resource> getDomParser() {
		return new ResourceDomParser();
	}

	@Override
	public SaxParser<Resource> getSaxParser() {
		throw new UnsupportedOperationException("Not yet implemented!"); //$NON-NLS-1$
	}
}