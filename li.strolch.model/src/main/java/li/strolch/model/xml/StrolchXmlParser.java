package li.strolch.model.xml;

import java.io.File;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;

public class StrolchXmlParser {

	public static Resource parseAndReturnResource(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getResource(id);
	}

	public static Order parseAndReturnOrder(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getOrder(id);
	}

	public static Activity parseAndReturnActivity(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getActivity(id);
	}
}
