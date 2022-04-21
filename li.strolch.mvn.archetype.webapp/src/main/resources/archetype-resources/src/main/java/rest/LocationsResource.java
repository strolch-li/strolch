package ${package}.rest;

import static ${package}.model.Constants.TYPE_LOCATION;
import static ${package}.model.JsonVisitors.locationToJson;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ${package}.search.LocationSearch;
import ${package}.service.RemoveLocationService;
import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.StringServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.collections.Paging;

@Path("locations")
public class LocationsResource {
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response query(@Context HttpServletRequest request, @QueryParam("query") String query,
			@DefaultValue("0") @QueryParam("offset") int offset, @DefaultValue("20") @QueryParam("limit") int limit) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// open the TX with the certificate, using this class as context
		Paging<JsonObject> paging;
		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getClass())) {

			// perform a book search
			paging = new LocationSearch() //
					.stringQuery(query) //
					.search(tx) //
					.orderByName(false) //
					.map(l -> l.accept(locationToJson(tx))) //
					.toPaging(offset, limit);
		}

		return ResponseUtil.toResponse(paging);
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
			Resource location = tx.getResourceBy(TYPE_LOCATION, id, true);

			// transform to JSON
			JsonObject locationJ = location.accept(locationToJson(tx));

			// return
			return ResponseUtil.toResponse(DATA, locationJ);
		}
	}

	@DELETE
	@Path("{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response remove(@Context HttpServletRequest request, @PathParam("id") String id) {

		// this is an authenticated method call, thus we can get the certificate from the request:
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// instantiate the service with the argument
		RemoveLocationService svc = new RemoveLocationService();
		StringServiceArgument arg = svc.getArgumentInstance();
		arg.value = id;

		// perform the service
		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		ServiceResult result = serviceHandler.doService(cert, svc, arg);

		// return depending on the result state
		return ResponseUtil.toResponse(result);
	}
}
