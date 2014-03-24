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
package li.strolch.model;

import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;
import li.strolch.model.xml.OrderToDomVisitor;
import li.strolch.model.xml.ResourceToDomVisitor;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
@SuppressWarnings("nls")
public class XmlToDomTest extends ModelTest {

    @Test
    public void shouldFormatAndParseOrder() {

        Order order = ModelGenerator.createOrder("@1", "My Order 1", "MyOrder");

        OrderToDomVisitor domVisitor = new OrderToDomVisitor();
        domVisitor.visit(order);
        Document document = domVisitor.getDocument();

        Element rootElement = document.getDocumentElement();
        Order parsedOrder = new Order(rootElement);

        OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(order);
        visitor.visit(parsedOrder);
        assertTrue("To DOM and back should equal same Order:\n" + visitor.getMismatchedLocators(),
                visitor.isEqual());
    }

    @Test
    public void shouldFormatAndParseResource() {

        Resource resource = ModelGenerator.createResource("@1", "My Resource 1", "MyResource");

        ResourceToDomVisitor domVisitor = new ResourceToDomVisitor();
        domVisitor.visit(resource);
        Document document = domVisitor.getDocument();

        Element rootElement = document.getDocumentElement();
        Resource parsedResource = new Resource(rootElement);

        ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(resource);
        visitor.visit(parsedResource);
        assertTrue("To DOM and back should equal same Resource:\n" + visitor.getMismatchedLocators(),
                visitor.isEqual());
    }
}
