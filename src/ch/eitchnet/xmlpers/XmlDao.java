/*
 * Copyright (c) 2010 - 2011
 * 
 * Apixxo AG 
 * Hauptgasse 25
 * 4600 Olten
 * 
 * All rights reserved.
 * 
 */
package ch.eitchnet.xmlpers;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.ContentHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface XmlDao<T> {

	/**
	 * @param object
	 * @return
	 */
	public String getType(T object);

	/**
	 * @param object
	 * @return
	 */
	public String getSubType(T object);

	/**
	 * @param object
	 * @return
	 */
	public String getId(T object);

	/**
	 * 
	 * @param object
	 * @param domImplementation
	 * @return
	 */
	// XXX think about returning a document, instead of an element, or use document as input
	public Document serializeToDom(T object, DOMImplementation domImplementation);

	/**
	 * @param element
	 * @return
	 */
	public T parseFromDom(Element element);

	/**
	 * @param object
	 * @param contentHandler
	 */
	// XXX Use the XMLSerializer object for serializing to SAX...
	public void serializeToSax(T object, ContentHandler contentHandler);

	// XXX parse from SAX is missing...

}
