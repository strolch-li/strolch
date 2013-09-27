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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import ch.eitchnet.xmlpers.api.XmlIoMode;

public class PersistenceContext<T> {

	private T object;
	private String type;
	private String subType;
	private String id;

	private XmlIoMode ioMode;
	private ParserFactory<T> parserFactory;

	public String getType() {
		return this.type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getSubType() {
		return this.subType;
	}

	public void setSubType(String subType) {
		this.subType = subType;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public T getObject() {
		return this.object;
	}

	public void setObject(T object) {
		this.object = object;
	}

	public XmlIoMode getIoMode() {
		return this.ioMode;
	}

	public void setIoMode(XmlIoMode ioMode) {
		this.ioMode = ioMode;
	}

	public ParserFactory<T> getParserFactor() {
		return this.parserFactory;
	}

	public void setParserFactory(ParserFactory<T> parserFactory) {
		this.parserFactory = parserFactory;
	}
}