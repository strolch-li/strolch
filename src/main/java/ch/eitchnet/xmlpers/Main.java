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
package ch.eitchnet.xmlpers;

import java.io.File;
import java.io.FileWriter;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class Main {

	public static void main(String[] args) throws Exception {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();

		File file = new File("output.xml");
		XMLStreamWriter writer = factory.createXMLStreamWriter(new FileWriter(file));
		System.out.println("writer: " + writer.getClass().getName());
		
		writer = new IndentingXMLStreamWriter(writer);

		writer.writeStartDocument();
		writer.writeStartElement("document");
		writer.writeEmptyElement("data");
		writer.writeAttribute("name", "value");
		//writer.writeEndElement();
		writer.writeEmptyElement("stuff");
		writer.writeAttribute("attr", "attrVal");
		//writer.writeEndElement();
		writer.writeEndDocument();

		writer.flush();
		writer.close();
		System.out.println("Wrote to " + file.getAbsolutePath());

		//Transformer transformer = TransformerFactory.newInstance().newTransformer();
		//Result outputTarget = new StaxR;
		//Source xmlSource;
		//transformer.transform(xmlSource, outputTarget);
	}
}
