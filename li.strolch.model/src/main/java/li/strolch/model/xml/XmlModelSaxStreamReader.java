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

import static li.strolch.model.StrolchModelConstants.DEFAULT_ENCODING;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.Date;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlModelSaxStreamReader extends XmlModelSaxReader {

	private InputSource source;

	public XmlModelSaxStreamReader(StrolchElementListener listener, InputStream stream) {
		this(listener, stream, DEFAULT_ENCODING);
	}

	public XmlModelSaxStreamReader(StrolchElementListener listener, InputStreamReader reader) {
		this(listener, reader, DEFAULT_ENCODING);
	}

	public XmlModelSaxStreamReader(StrolchElementListener listener, InputStream stream, String encoding) {
		super(listener);

		try {
			this.source = new InputSource(new InputStreamReader(stream, encoding));
			this.source.setEncoding(encoding);
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException("Encoding " + encoding + " unsupported!");
		}
	}

	public XmlModelSaxStreamReader(StrolchElementListener listener, InputStreamReader reader, String encoding) {
		super(listener);

		this.source = new InputSource(reader);
		this.source.setEncoding(encoding);
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {

		case Tags.INCLUDE_FILE:
			String msg = "The {0} can''t handle Tags of type {1}";
			msg = MessageFormat.format(msg, XmlModelSaxStreamReader.class.getName(), Tags.INCLUDE_FILE);
			throw new IllegalArgumentException(msg); //$NON-NLS-1$
		default:
			super.startElement(uri, localName, qName, attributes);
		}
	}

	public void parseStream() {

		try {
			long startNanos = System.nanoTime();
			this.statistics.startTime = LocalDateTime.now();

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			sp.parse(this.source, this);

			long endNanos = System.nanoTime();
			this.statistics.durationNanos = endNanos - startNanos;
			String msg = "SAX parsed stream took {0}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, StringHelper.formatNanoDuration(this.statistics.durationNanos)));

		} catch (ParserConfigurationException | SAXException | IOException e) {

			String msg = "Parsing failed due to internal error: {0}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, e.getMessage()), e);
		}
	}
}
