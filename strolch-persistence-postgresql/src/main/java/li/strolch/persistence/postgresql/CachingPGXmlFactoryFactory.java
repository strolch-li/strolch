package li.strolch.persistence.postgresql;

import li.strolch.utils.helper.XmlHelper;
import org.postgresql.xml.EmptyStringEntityResolver;
import org.postgresql.xml.NullErrorHandler;
import org.postgresql.xml.PGXmlFactoryFactory;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXTransformerFactory;

public class CachingPGXmlFactoryFactory implements PGXmlFactoryFactory {

	private DocumentBuilder documentBuilder;
	private TransformerFactory transformerFactory;
	private SAXTransformerFactory saxTransformerFactory;
	private XMLInputFactory xmlInputFactory;
	private XMLOutputFactory xmlOutputFactory;
	private XMLReader xmlReader;

	@Override
	public synchronized DocumentBuilder newDocumentBuilder() throws ParserConfigurationException {
		if (this.documentBuilder != null)
			return this.documentBuilder;

		this.documentBuilder = XmlHelper.getDocumentBuilder();
		this.documentBuilder.setEntityResolver(EmptyStringEntityResolver.INSTANCE);
		this.documentBuilder.setErrorHandler(NullErrorHandler.INSTANCE);
		return this.documentBuilder;
	}

	@Override
	public synchronized TransformerFactory newTransformerFactory() {
		if (this.transformerFactory != null)
			return this.transformerFactory;

		this.transformerFactory = TransformerFactory.newInstance();
		setFactoryProperties(this.transformerFactory);
		return this.transformerFactory;
	}

	@Override
	public synchronized SAXTransformerFactory newSAXTransformerFactory() {
		if (this.saxTransformerFactory != null)
			return this.saxTransformerFactory;

		this.saxTransformerFactory = (SAXTransformerFactory) SAXTransformerFactory.newInstance();
		setFactoryProperties(this.saxTransformerFactory);
		return this.saxTransformerFactory;
	}

	@Override
	public synchronized XMLInputFactory newXMLInputFactory() {
		if (this.xmlInputFactory != null)
			return this.xmlInputFactory;

		this.xmlInputFactory = XMLInputFactory.newInstance();
		setPropertyQuietly(this.xmlInputFactory, XMLInputFactory.SUPPORT_DTD, false);
		setPropertyQuietly(this.xmlInputFactory, XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);
		return this.xmlInputFactory;
	}

	@Override
	public synchronized XMLOutputFactory newXMLOutputFactory() {
		if (this.xmlOutputFactory != null)
			return this.xmlOutputFactory;

		this.xmlOutputFactory = XMLOutputFactory.newInstance();
		return this.xmlOutputFactory;
	}

	@Override
	public synchronized XMLReader createXMLReader() throws SAXException {
		if (this.xmlReader != null)
			return this.xmlReader;

		this.xmlReader = XMLReaderFactory.createXMLReader();
		setFeatureQuietly(this.xmlReader, "http://apache.org/xml/features/disallow-doctype-decl", true);
		setFeatureQuietly(this.xmlReader, "http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
		setFeatureQuietly(this.xmlReader, "http://xml.org/sax/features/external-general-entities", false);
		setFeatureQuietly(this.xmlReader, "http://xml.org/sax/features/external-parameter-entities", false);
		this.xmlReader.setErrorHandler(NullErrorHandler.INSTANCE);
		return this.xmlReader;
	}

	private static void setFeatureQuietly(Object factory, String name, boolean value) {
		try {
			switch (factory) {
				case DocumentBuilderFactory f -> f.setFeature(name, value);
				case TransformerFactory f -> f.setFeature(name, value);
				case XMLReader r -> r.setFeature(name, value);
				default -> throw new Error("Invalid factory class: " + factory.getClass());
			}
		} catch (Exception ignore) {
			// ignore
		}
	}

	private static void setAttributeQuietly(Object factory, String name, Object value) {
		try {
			switch (factory) {
				case DocumentBuilderFactory f -> f.setAttribute(name, value);
				case TransformerFactory f -> f.setAttribute(name, value);
				default -> throw new Error("Invalid factory class: " + factory.getClass());
			}
		} catch (Exception ignore) {
			// ignore
		}
	}

	private static void setPropertyQuietly(Object factory, String name, Object value) {
		try {
			switch (factory) {
				case XMLReader r -> r.setProperty(name, value);
				case XMLInputFactory f -> f.setProperty(name, value);
				default -> throw new Error("Invalid factory class: " + factory.getClass());
			}
		} catch (Exception ignore) {
			// ignore
		}
	}

	private static void setFactoryProperties(Object factory) {
		setFeatureQuietly(factory, XMLConstants.FEATURE_SECURE_PROCESSING, true);
		setFeatureQuietly(factory, "http://apache.org/xml/features/disallow-doctype-decl", true);
		setFeatureQuietly(factory, "http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
		setFeatureQuietly(factory, "http://xml.org/sax/features/external-general-entities", false);
		setFeatureQuietly(factory, "http://xml.org/sax/features/external-parameter-entities", false);
		// Values from XMLConstants inlined for JDK 1.6 compatibility
		setAttributeQuietly(factory, "http://javax.xml.XMLConstants/property/accessExternalDTD", "");
		setAttributeQuietly(factory, "http://javax.xml.XMLConstants/property/accessExternalSchema", "");
		setAttributeQuietly(factory, "http://javax.xml.XMLConstants/property/accessExternalStylesheet", "");
	}
}
