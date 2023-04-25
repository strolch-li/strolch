package li.strolch.utils.helper;

import static org.junit.Assert.assertEquals;

import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.junit.BeforeClass;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlSignHelperTest {

	private static XmlDomSigner helper;

	@BeforeClass
	public static void beforeClass() {
		helper = new XmlDomSigner(new File("src/test/resources/test.jks"), "client", "server",
				"changeit".toCharArray());
	}

	@Test
	public void shouldSign() {
		Document document = createDoc();
		helper.sign(document);

		assertSignatureElemExists(document);
	}

	@Test
	public void shouldSignWithNamespaces() {

		Document document = createDocWithNamespaces();

		// hack for signing with namespaces problem
		document = XmlDomSigner.parse(XmlDomSigner.transformToBytes(document));

		helper.sign(document);

		assertSignatureElemExists(document);
	}

	private void assertSignatureElemExists(Document document) {
		NodeList nl = document.getElementsByTagNameNS(XMLSignature.XMLNS, "Signature");
		assertEquals("Expected exactly one Signature element!", 1, nl.getLength());
	}

	@Test
	public void shouldValidate() {

		File signedXmlFile = new File("src/test/resources/SignedXmlFile.xml");
		Document document = XmlDomSigner.parse(signedXmlFile);
		setIdAttr(document);
		helper.validate(document);
	}

	@Test
	public void shouldValidateWithNamespaces() {

		File signedXmlFile = new File("src/test/resources/SignedXmlFileWithNamespaces.xml");
		Document document = XmlDomSigner.parse(signedXmlFile);
		setIdAttrNs(document);

		helper.validate(document);
	}

	private void setIdAttr(Document document) {
		NodeList authnRequestNodes = document.getElementsByTagName("AuthnRequest");
		if (authnRequestNodes.getLength() != 1)
			throw new IllegalStateException("Multiple or no AuthnRequest Node found in document!");
		Element authnRequestNode = (Element) authnRequestNodes.item(0);
		authnRequestNode.setIdAttribute("ID", true);
	}

	private void setIdAttrNs(Document document) {
		NodeList authnRequestNodes = document
				.getElementsByTagNameNS("urn:oasis:names:tc:SAML:2.0:protocol", "AuthnRequest");
		if (authnRequestNodes.getLength() != 1)
			throw new IllegalStateException("Multiple or no AuthnRequest Node found in document!");
		Element authnRequestNode = (Element) authnRequestNodes.item(0);
		authnRequestNode.setIdAttribute("ID", true);
	}

	@Test
	public void shouldSignAndValidate() {

		Document document = createDoc();

		helper.sign(document);
		helper.validate(document);
	}

	@Test
	public void shouldSignAndValidateWithNamespaces() {

		Document document = createDocWithNamespaces();

		// hack for signing with namespaces problem
		document = XmlDomSigner.parse(XmlDomSigner.transformToBytes(document));

		helper.sign(document);
		helper.validate(document);
	}

	public static Document createDoc() {

		String issuer = "test";
		String destination = "test";
		String assertionConsumerServiceUrl = "test";
		Calendar issueInstant = Calendar.getInstance();

		// create dates
		SimpleDateFormat simpleDf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
		simpleDf.setTimeZone(TimeZone.getTimeZone("UTC"));
		String issueInstantS = simpleDf.format(issueInstant.getTime());
		issueInstant.add(Calendar.SECOND, 10);
		String notOnOrAfterS = simpleDf.format(issueInstant.getTime());

		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		DocumentBuilder docBuilder;
		try {
			docBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new RuntimeException("Failed to configure document builder!", e);
		}
		Document doc = docBuilder.newDocument();

		Element authnReqE = doc.createElement("AuthnRequest");
		authnReqE.setAttribute("Version", "2.0");
		authnReqE.setAttribute("IssueInstant", issueInstantS);
		authnReqE.setAttribute("ProtocolBinding", "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST");
		authnReqE.setAttribute("AssertionConsumerServiceURL", assertionConsumerServiceUrl);
		authnReqE.setAttribute("Destination", destination);
		doc.appendChild(authnReqE);

		Element issuerE = doc.createElement("Issuer");
		issuerE.setTextContent(issuer);
		authnReqE.appendChild(issuerE);

		Element conditionsE = doc.createElement("Conditions");
		conditionsE.setAttribute("NotBefore", issueInstantS);
		conditionsE.setAttribute("NotOnOrAfter", notOnOrAfterS);
		authnReqE.appendChild(conditionsE);

		return doc;
	}

	public static Document createDocWithNamespaces() {

		String issuer = "test";
		String destination = "test";
		String assertionConsumerServiceUrl = "test";
		Calendar issueInstant = Calendar.getInstance();

		// create dates
		SimpleDateFormat simpleDf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
		simpleDf.setTimeZone(TimeZone.getTimeZone("UTC"));
		String issueInstantS = simpleDf.format(issueInstant.getTime());
		issueInstant.add(Calendar.SECOND, 10);
		String notOnOrAfterS = simpleDf.format(issueInstant.getTime());

		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		DocumentBuilder docBuilder;
		try {
			docBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new RuntimeException("Failed to configure document builder!", e);
		}
		Document doc = docBuilder.newDocument();

		Element authnReqE = doc.createElementNS("urn:oasis:names:tc:SAML:2.0:protocol", "AuthnRequest");
		authnReqE.setPrefix("samlp");
		authnReqE.setAttribute("Version", "2.0");
		authnReqE.setAttribute("IssueInstant", issueInstantS);
		authnReqE.setAttribute("ProtocolBinding", "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST");
		authnReqE.setAttribute("AssertionConsumerServiceURL", assertionConsumerServiceUrl);
		authnReqE.setAttribute("Destination", destination);
		doc.appendChild(authnReqE);

		Element issuerE = doc.createElementNS("urn:oasis:names:tc:SAML:2.0:assertion", "Issuer");
		issuerE.setPrefix("saml");
		issuerE.setTextContent(issuer);
		authnReqE.appendChild(issuerE);

		Element conditionsE = doc.createElementNS("urn:oasis:names:tc:SAML:2.0:assertion", "Conditions");
		conditionsE.setPrefix("saml");
		conditionsE.setAttribute("NotBefore", issueInstantS);
		conditionsE.setAttribute("NotOnOrAfter", notOnOrAfterS);
		authnReqE.appendChild(conditionsE);

		return doc;
	}
}
