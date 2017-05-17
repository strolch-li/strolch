package li.strolch.model;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.model.xml.StrolchElementToXmlStringVisitor;

public abstract class AbstractStrolchRootElement extends GroupedParameterizedElement implements StrolchRootElement {

	private static final long serialVersionUID = 1L;

	public AbstractStrolchRootElement() {
		super();
	}

	public AbstractStrolchRootElement(String id, String name, String type) {
		super(id, name, type);
	}

	@Override
	public String toXmlString() {
		return accept(new StrolchElementToXmlStringVisitor());
	}

	@Override
	public String toJsonString() {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(this.accept(new StrolchElementToJsonVisitor()));
	}

	@Override
	public String toFlatJsonString() {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(this.accept(new StrolchElementToJsonVisitor().flat()));
	}
}
