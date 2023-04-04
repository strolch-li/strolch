package li.strolch.utils.helper;

import javax.xml.crypto.dsig.*;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.X509Data;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.Files;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStore.TrustedCertificateEntry;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.cert.X509Certificate;
import java.util.*;

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class XmlDomSigner {

	private static final Logger logger = LoggerFactory.getLogger(XmlDomSigner.class);

	private final KeyStore keyStore;
	private final String privateKeyAlias;
	private final String trustAlias;

	private final char[] password;

	public XmlDomSigner(File keyStorePath, String privateKeyAlias, String trustAlias, char[] password) {

		DBC.PRE.assertNotEmpty("privateKeyAlias", privateKeyAlias);
		DBC.PRE.assertNotEmpty("trustAlias", trustAlias);
		try {

			KeyStore keyStore = KeyStore.getInstance("JKS");
			keyStore.load(Files.newInputStream(keyStorePath.toPath()), password);
			this.keyStore = keyStore;
			this.privateKeyAlias = privateKeyAlias;
			this.trustAlias = trustAlias;
			this.password = password;

		} catch (Exception e) {
			throw new RuntimeException("Failed to read keystore " + keyStorePath);
		}
	}

	public void sign(Document document) throws RuntimeException {

		try {

			String id = "Signed_" + UUID.randomUUID();
			Element rootElement = document.getDocumentElement();
			rootElement.setAttribute("ID", id);
			rootElement.setIdAttribute("ID", true);

			// Create a DOM XMLSignatureFactory that will be used to
			// generate the enveloped signature.
			XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");

			// Create a Reference to the enveloped document (in this case,
			// you are signing the whole document, so a URI of "" signifies
			// that, and also specify the SHA1 digest algorithm and
			// the ENVELOPED Transform.
			List<Transform> transforms = new ArrayList<>();
			transforms.add(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec) null));
			transforms.add(fac.newTransform(CanonicalizationMethod.EXCLUSIVE, (TransformParameterSpec) null));
			DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA256, null);
			Reference ref = fac.newReference("#" + id, digestMethod, transforms, null, null);

			// Create the SignedInfo.
			SignedInfo signedInfo = fac.newSignedInfo(
					fac.newCanonicalizationMethod(CanonicalizationMethod.EXCLUSIVE, (C14NMethodParameterSpec) null), //
					fac.newSignatureMethod(SignatureMethod.RSA_SHA256, null), //
					Collections.singletonList(ref));

			// Load the KeyStore and get the signing key and certificate.
			PrivateKeyEntry keyEntry = (PrivateKeyEntry) this.keyStore.getEntry(this.privateKeyAlias,
					new KeyStore.PasswordProtection(this.password));
			PrivateKey privateKey = keyEntry.getPrivateKey();
			X509Certificate cert = (X509Certificate) keyEntry.getCertificate();

			// Create the KeyInfo containing the X509Data.
			KeyInfoFactory kif = fac.getKeyInfoFactory();
			List<Object> x509Content = new ArrayList<>();

			x509Content.add(cert.getSubjectX500Principal().getName());
			x509Content.add(cert);
			X509Data xd = kif.newX509Data(x509Content);
			KeyInfo keyInfo = kif.newKeyInfo(Collections.singletonList(xd));

			// Create a DOMSignContext and specify the RSA PrivateKey and
			// location of the resulting XMLSignature's parent element.
			DOMSignContext dsc = new DOMSignContext(privateKey, rootElement);
			//dsc.setDefaultNamespacePrefix("samlp");
			dsc.putNamespacePrefix(XMLSignature.XMLNS, "ds");

			// Create the XMLSignature, but don't sign it yet.
			XMLSignature signature = fac.newXMLSignature(signedInfo, keyInfo);

			// Marshal, generate, and sign the enveloped signature.
			signature.sign(dsc);

		} catch (Exception e) {
			throw new RuntimeException("Failed to sign document", e);
		}
	}

	public void validate(Document doc) throws RuntimeException {

		try {

			// Create a DOM XMLSignatureFactory that will be used to
			// generate the enveloped signature.
			XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");

			// Find Signature element.
			NodeList nl = doc.getElementsByTagNameNS(XMLSignature.XMLNS, "Signature");
			if (nl.getLength() == 0) {
				throw new Exception("Cannot find Signature element!");
			} else if (nl.getLength() > 1) {
				throw new Exception("Found multiple Signature elements!");
			}

			// Load the KeyStore and get the signing key and certificate.
			TrustedCertificateEntry entry = (TrustedCertificateEntry) this.keyStore.getEntry(trustAlias, null);
			PublicKey publicKey = entry.getTrustedCertificate().getPublicKey();

			// Create a DOMValidateContext and specify a KeySelector
			// and document context.
			Node signatureNode = nl.item(0);
			DOMValidateContext valContext = new DOMValidateContext(publicKey, signatureNode);

			// Unmarshal the XMLSignature.
			valContext.setProperty("javax.xml.crypto.dsig.cacheReference", Boolean.TRUE);
			valContext.setProperty("org.jcp.xml.dsig.secureValidation", Boolean.FALSE);
			XMLSignature signature = fac.unmarshalXMLSignature(valContext);

			// Validate the XMLSignature.
			boolean coreValidity = signature.validate(valContext);

			// Check core validation status.
			if (!coreValidity) {
				logger.error("Signature failed core validation");
				boolean sv = signature.getSignatureValue().validate(valContext);
				logger.error("signature validation status: " + sv);
				if (!sv) {
					// Check the validation status of each Reference.
					Iterator<?> i = signature.getSignedInfo().getReferences().iterator();
					for (int j = 0; i.hasNext(); j++) {
						boolean refValid = ((Reference) i.next()).validate(valContext);
						logger.error("ref[" + j + "] validity status: " + refValid);
					}
				}
				throw new RuntimeException("Uh-oh validation, failed!");
			}
		} catch (Exception e) {
			if (e instanceof RuntimeException)
				throw (RuntimeException) e;
			throw new RuntimeException("Failed to validate document", e);
		}
	}

	public static byte[] transformToBytes(Document doc) {
		return transformToBytes(doc, false);
	}

	public static byte[] transformToBytes(Document doc, boolean indent) {
		try {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();

			if (indent) {
				transformer.setOutputProperty(OutputKeys.INDENT, "yes");
				transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount",
						"2"); //$NON-NLS-2$
			}

			transformer.transform(new DOMSource(doc), new StreamResult(out));
			return out.toByteArray();
		} catch (TransformerFactoryConfigurationError | TransformerException e) {
			throw new RuntimeException("Failed to transform document to bytes!", e);
		}
	}

	public static void writeTo(Document doc, File file) {
		try {
			writeTo(doc, Files.newOutputStream(file.toPath()));
		} catch (IOException e) {
			throw new RuntimeException("Failed to write document to " + file.getAbsolutePath(), e);
		}
	}

	public static void writeTo(Document doc, OutputStream out) {
		try {
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();
			transformer.transform(new DOMSource(doc), new StreamResult(out));
		} catch (Exception e) {
			throw new RuntimeException("Failed to write document to output stream!", e);
		}
	}

	public static Document parse(byte[] bytes) {
		return parse(new ByteArrayInputStream(bytes));
	}

	public static Document parse(File signedXmlFile) {
		try {
			return parse(Files.newInputStream(signedXmlFile.toPath()));
		} catch (Exception e) {
			throw new RuntimeException("Failed to parse signed file at " + signedXmlFile.getAbsolutePath(), e);
		}
	}

	public static Document parse(InputStream in) {

		Document doc;
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			doc = dbf.newDocumentBuilder().parse(in);
		} catch (Exception e) {
			throw new RuntimeException("Failed to parse input stream", e);
		}

		return doc;
	}
}
