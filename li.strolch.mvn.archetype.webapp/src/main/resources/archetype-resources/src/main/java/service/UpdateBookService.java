package ${package}.service;

import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;

import ${package}.model.Constants;
import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.Tags.Json;
import li.strolch.model.json.FromFlatJsonVisitor;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.api.AbstractService;
import li.strolch.utils.dbc.DBC;

public class UpdateBookService extends AbstractService<JsonServiceArgument, JsonServiceResult> {

	@Override
	protected JsonServiceResult getResultInstance() {
		return new JsonServiceResult();
	}

	@Override
	public JsonServiceArgument getArgumentInstance() {
		return new JsonServiceArgument();
	}

	@Override
	protected JsonServiceResult internalDoService(JsonServiceArgument arg) throws Exception {

		// verify same book
		DBC.PRE.assertEquals("ObjectId and given Id must be same!", arg.objectId,
				arg.jsonElement.getAsJsonObject().get(Json.ID).getAsString());

		// open a new transaction, using the realm from the argument, or the certificate
		Resource book;
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			// get the existing book
			book = tx.getResourceBy(Constants.TYPE_BOOK, arg.objectId, true);

			// map all values from the JSON object into the new book element
			book.accept(new FromFlatJsonVisitor(arg.jsonElement.getAsJsonObject()).ignoreBag(BAG_RELATIONS));

			// save changes
			tx.update(book);

			// notify the TX that it should commit on close
			tx.commitOnClose();
		}

		// map the return value to JSON
		JsonObject result = book.accept(new StrolchRootElementToJsonVisitor().flat());

		// and return the result
		return new JsonServiceResult(result);
	}
}
