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

import static li.strolch.utils.helper.StringHelper.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.File;
import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.Date;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlModelSaxFileReader extends XmlModelSaxReader {

	private final File modelFile;
	private final boolean allowInclude;

	public XmlModelSaxFileReader(StrolchElementListener listener, File modelFile, boolean allowInclude) {
		super(listener);
		this.modelFile = modelFile;
		this.allowInclude = allowInclude;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		if (!Tags.INCLUDE_FILE.equals(qName)) {
			super.startElement(uri, localName, qName, attributes);
		} else {
			if (!this.allowInclude) {
				String msg = "ModelFile {0} has includes which are disabled for this parse invocation!";
				throw new IllegalArgumentException(MessageFormat.format(msg, this.modelFile.getAbsolutePath()));
			}

			String includeFileS = attributes.getValue(Tags.FILE);
			if (isEmpty(includeFileS)) {
				throw new IllegalArgumentException(
						MessageFormat.format("The attribute {0} is missing for IncludeFile!", Tags.FILE));
			}

			File includeFile = new File(this.modelFile.getParentFile(), includeFileS);
			if (!includeFile.exists() || !includeFile.canRead()) {
				String msg = "The IncludeFile does not exist, or is not readable. Source model: {0} with IncludeFile: {1}";
				msg = MessageFormat.format(msg, this.modelFile.getAbsolutePath(), includeFileS);
				throw new IllegalArgumentException(msg);
			}

			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(this.listener, includeFile, true);
			handler.parseFile();
			this.statistics.add(handler.statistics);
		}
	}

	public void parseFile() {

		try {
			long startNanos = System.nanoTime();
			this.statistics.startTime = LocalDateTime.now();

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			sp.parse(this.modelFile, this);

			long endNanos = System.nanoTime();
			this.statistics.durationNanos = endNanos - startNanos;
			String msg = "SAX parsed model file {0} took {1}";
			logger.info(MessageFormat.format(msg, this.modelFile.getAbsolutePath(),
					formatNanoDuration(this.statistics.durationNanos)));

		} catch (Exception e) {

			String msg = "Parsing of {0} failed due to internal error: {1}";
			throw new StrolchException(MessageFormat.format(msg, this.modelFile.getAbsolutePath(), e.getMessage()), e);
		}
	}
}
