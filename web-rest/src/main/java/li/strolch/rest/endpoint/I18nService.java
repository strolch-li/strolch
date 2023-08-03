package li.strolch.rest.endpoint;

import static li.strolch.model.StrolchModelConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.persistence.api.Operation;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("strolch/i18n")
public class I18nService {

	private static final Logger logger = LoggerFactory.getLogger(I18nService.class);

	@GET
	@Path("data")
	@Produces(MediaType.TEXT_PLAIN)
	public Response getI18nDataPlain(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		PrivilegeContext ctx = RestfulStrolchComponent.getInstance().validate(cert);
		if (!ctx.hasRole(ROLE_STROLCH_ADMIN))
			ctx.validateAction(new SimpleRestrictable("I18n", Operation.GET.getPrivilegePrefix()));

		File webPath = new File(RestfulStrolchComponent.getInstance().getWebPath());
		File i18nFile = new File(webPath, "/locales.json");

		JsonObject i18nJ;
		try (FileReader in = new FileReader(i18nFile)) {
			i18nJ = JsonParser.parseReader(in).getAsJsonObject();
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read i18n file at " + i18nFile);
		}

		logger.info("Returning I18n data to user " + cert.getUsername());
		String response = new GsonBuilder().setPrettyPrinting().create().toJson(i18nJ);
		return Response.ok(response, MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Path("data")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getI18nData(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		PrivilegeContext ctx = RestfulStrolchComponent.getInstance().validate(cert);
		if (!ctx.hasRole(ROLE_STROLCH_ADMIN))
			ctx.validateAction(new SimpleRestrictable("I18n", Operation.GET.getPrivilegePrefix()));

		File webPath = new File(RestfulStrolchComponent.getInstance().getWebPath());
		File i18nFile = new File(webPath, "/locales.json");

		JsonObject i18nJ;
		try (FileReader in = new FileReader(i18nFile)) {
			i18nJ = JsonParser.parseReader(in).getAsJsonObject();
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read i18n file at " + i18nFile);
		}

		logger.info("Returning I18n data to user " + cert.getUsername());
		return ResponseUtil.toResponse(DATA, i18nJ);
	}

	@PUT
	@Path("data")
	@Consumes(MediaType.APPLICATION_JSON)
	public Response setI18nData(@Context HttpServletRequest request, String data) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		PrivilegeContext ctx = RestfulStrolchComponent.getInstance().validate(cert);
		if (!ctx.hasRole(ROLE_STROLCH_ADMIN))
			ctx.validateAction(new SimpleRestrictable("I18n", Operation.UPDATE.getPrivilegePrefix()));

		JsonObject i18nJ = JsonParser.parseString(data).getAsJsonObject();

		File webPath = new File(RestfulStrolchComponent.getInstance().getWebPath());
		File i18nFile = new File(webPath, "/locales.json");
		File i18nBackupFile = new File(webPath, "/locales.json_" + System.currentTimeMillis());

		if (i18nFile.exists() && !i18nFile.canWrite())
			throw new IllegalStateException("Can not write to " + i18nFile);

		if (i18nFile.exists() && !i18nFile.renameTo(i18nBackupFile))
			throw new IllegalStateException("Failed to create backup of i18n file to " + i18nBackupFile);

		logger.info("Overwriting I18n data by user " + cert.getUsername() + " to file " + i18nFile);
		try (FileWriter out = new FileWriter(i18nFile)) {
			new GsonBuilder().setPrettyPrinting().create().toJson(i18nJ, out);
		} catch (IOException e) {
			if (i18nBackupFile.exists()) {
				if (i18nFile.exists() && !i18nFile.delete())
					throw new IllegalStateException(
							"Failed to restore backup after failing to write i18n file to " + i18nBackupFile, e);
				if (!i18nBackupFile.renameTo(i18nFile))
					throw new IllegalStateException(
							"Failed to restore backup after failing to write i18n file to " + i18nBackupFile, e);
			}

			throw new IllegalStateException("Failed to write i18n file to " + i18nFile, e);
		}

		return ResponseUtil.toResponse();
	}
}
