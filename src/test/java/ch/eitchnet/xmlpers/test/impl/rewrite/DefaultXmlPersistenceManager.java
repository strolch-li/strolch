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

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.impl.XmlPersistenceHandlerImpl;
import ch.eitchnet.xmlpers.impl.XmlPersistencePathBuilder;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultXmlPersistenceManager implements XmlPersistenceManager {

	protected static final Logger logger = LoggerFactory.getLogger(XmlPersistenceHandlerImpl.class);

	protected boolean initialized;
	protected boolean verbose;

	private XmlPersistencePathBuilder pathBuilder;

	public void initialize(Properties properties) {
		if (this.initialized)
			throw new IllegalStateException("Already initialized!"); //$NON-NLS-1$

		// get properties
		String context = XmlPersistenceHandlerImpl.class.getSimpleName();
		boolean verbose = PropertiesHelper.getPropertyBool(properties, context, XmlPersistenceConstants.PROP_VERBOSE,
				Boolean.FALSE).booleanValue();

		this.verbose = verbose;

		this.pathBuilder = new XmlPersistencePathBuilder(properties);
	}

	@Override
	public PersistenceTransaction openTx() {

		FileDao fileDao = new FileDao(this.pathBuilder, this.verbose);
		PersistenceTransaction tx = new DefaultPersistenceTransaction(fileDao, this.verbose);

		return tx;
	}
}
