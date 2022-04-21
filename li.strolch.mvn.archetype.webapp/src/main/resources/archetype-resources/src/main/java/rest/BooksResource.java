package ${package}.rest;

import static ${package}.model.Constants.*;
import static ${package}.model.JsonVisitors.bookToJson;
import static li.strolch.rest.StrolchRestfulConstants.*;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import ${package}.model.Constants;
import ${package}.model.JsonVisitors;
import ${package}.search.BookSearch;
import ${package}.service.CreateBookService;
import ${package}.service.RemoveBookService;
import ${package}.service.UpdateBookService;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.model.Resource;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.visitor.ResourceVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.StringServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.collections.Paging;

@Path("books")
public class BooksResource {

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response query(@Context HttpServletRequest request, @QueryParam("query") String query,
			@DefaultValue("0") @QueryParam("offset") int offset, @DefaultValue("20") @QueryParam("limit") int limit) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// open the TX with the certificate, using this class as context
		Paging<Resource> paging;
		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getClass())) {

			// perform a book search
			paging = new BookSearch() //
					.stringQuery(query) //
					.search(tx) //
					.orderByName(false) //
					.toPaging(offset, limit);
		}

		return ResponseUtil.toResponse(paging, e -> e.accept(bookToJson()));
	}

	@GET
	@Path("{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response get(@Context HttpServletRequest request, @PathParam("id") String id) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// open the TX with the certificate, using this class as context
		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getClass())) {

			// get the book
			Resource book = tx.getResourceBy(TYPE_BOOK, id, true);

			// transform to JSON
			JsonObject bookJ = book.accept(bookToJson());

			// return
			return ResponseUtil.toResponse(DATA, bookJ);
		}
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	public Response create(@Context HttpServletRequest request, String data) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// parse data to JSON
		JsonObject jsonData = JsonParser.parseString(data).getAsJsonObject();

		// instantiate the service with the argument
		CreateBookService svc = new CreateBookService();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.jsonElement = jsonData;

		// perform the service
		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		JsonServiceResult result = serviceHandler.doService(cert, svc, arg);

		// return depending on the result state
		if (result.isOk())
			return ResponseUtil.toResponse(DATA, result.getResult());
		return ResponseUtil.toResponse(result);
	}

	@PUT
	@Path("{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response update(@Context HttpServletRequest request, @PathParam("id") String id, String data) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// parse data to JSON
		JsonObject jsonData = JsonParser.parseString(data).getAsJsonObject();

		// instantiate the service with the argument
		UpdateBookService svc = new UpdateBookService();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.objectId = id;
		arg.jsonElement = jsonData;

		// perform the service
		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		JsonServiceResult result = serviceHandler.doService(cert, svc, arg);

		// return depending on the result state
		if (result.isOk())
			return ResponseUtil.toResponse(DATA, result.getResult());
		return ResponseUtil.toResponse(result);
	}

	@DELETE
	@Path("{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response remove(@Context HttpServletRequest request, @PathParam("id") String id) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// instantiate the service with the argument
		RemoveBookService svc = new RemoveBookService();
		StringServiceArgument arg = svc.getArgumentInstance();
		arg.value = id;

		// perform the service
		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		ServiceResult result = serviceHandler.doService(cert, svc, arg);

		// return depending on the result state
		return ResponseUtil.toResponse(result);
	}
}
