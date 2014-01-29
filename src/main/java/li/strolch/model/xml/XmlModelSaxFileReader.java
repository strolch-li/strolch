/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.model.xml;

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Date;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlModelSaxFileReader extends XmlModelSaxReader {

	private File modelFile;

	/**
	 * @param listener
	 * @param modelFile
	 */
	public XmlModelSaxFileReader(StrolchElementListener listener, File modelFile) {
		super(listener);
		this.modelFile = modelFile;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {

		case Tags.INCLUDE_FILE:

			String includeFileS = attributes.getValue(Tags.FILE);
			if (StringHelper.isEmpty(includeFileS))
				throw new IllegalArgumentException(MessageFormat.format(
						"The attribute {0} is missing for IncludeFile!", Tags.FILE)); //$NON-NLS-1$
			File includeFile = new File(this.modelFile.getParentFile(), includeFileS);
			if (!includeFile.exists() || !includeFile.canRead()) {
				String msg = "The IncludeFile does not exist, or is not readable. Source model: {0} with IncludeFile: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.modelFile.getName(), includeFileS);
				throw new IllegalArgumentException(msg);
			}

			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(this.listener, includeFile);
			handler.parseFile();
			this.statistics.nrOfOrders += handler.statistics.nrOfOrders;
			this.statistics.nrOfResources += handler.statistics.nrOfResources;

			break;
		default:
			super.startElement(uri, localName, qName, attributes);
		}
	}

	public void parseFile() {

		try {
			long startNanos = System.nanoTime();
			this.statistics.startTime = new Date();

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			sp.parse(this.modelFile, this);

			long endNanos = System.nanoTime();
			this.statistics.durationNanos = endNanos - startNanos;
			String msg = "SAX parsed model file {0} took {1}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.modelFile.getAbsolutePath(),
					StringHelper.formatNanoDuration(this.statistics.durationNanos)));

		} catch (ParserConfigurationException | SAXException | IOException e) {

			String msg = "Parsing failed due to internal error: {0}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, e.getMessage()), e);
		}
	}
}
