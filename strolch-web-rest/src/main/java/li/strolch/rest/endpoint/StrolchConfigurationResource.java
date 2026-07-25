/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.model.Resource;
import li.strolch.model.json.StrolchElementFromJsonVisitor;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.ConfigurationPolicy;
import li.strolch.policy.StrolchPolicyFileParser.PolicyModel;
import li.strolch.policy.StrolchPolicyFileParser.PolicyType;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.StrolchResponse;

import java.util.Map;

import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

@Path("strolch/configuration")
@Tag(name = "Configuration", description = "API to view and modify Strolch configuration and policies")
public class StrolchConfigurationResource {

	private static Certificate validateCertificate(HttpServletRequest request, String action) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(new SimpleRestrictable("StrolchConfiguration", action));
		return cert;
	}

	@Operation(summary = "Get configuration resource", description = "Retrieves the Strolch configuration resource.",
			responses = {@ApiResponse(responseCode = "200", description = "Configuration resource retrieved.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("resource")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getConfigurationResource(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request, "GetConfiguration");
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();

		try (StrolchTransaction tx = rest.openTx(cert, getClass())) {
			ConfigurationPolicy policy = ConfigurationPolicy.getDefaultPolicy(tx);
			Resource resource = policy.getConfigurationResource();
			JsonElement json = resource.accept(new StrolchElementToJsonVisitor());
			return ResponseUtil.toResponse(DATA, json);
		}
	}

	@Operation(summary = "Update configuration resource", description = "Updates the Strolch configuration resource.",
			responses = {
					@ApiResponse(responseCode = "200", description = "Configuration resource updated successfully.",
							content = @Content(mediaType = "application/json",
									schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Path("resource")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response updateConfigurationResource(@Context HttpServletRequest request, String data) {
		Certificate cert = validateCertificate(request, "UpdateConfiguration");
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();

		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		try (StrolchTransaction tx = rest.openTx(cert, getClass(), false)) {
			ConfigurationPolicy policy = ConfigurationPolicy.getDefaultPolicy(tx);
			Resource resource = policy.getConfigurationResource();
			new StrolchElementFromJsonVisitor().fillElement(jsonObject, resource);
			policy.updateConfigurationResource(resource);
			tx.commitOnClose();
		}

		return ResponseUtil.toResponse();
	}

	@Operation(summary = "Get policies", description = "Retrieves the current policy model.", responses = {
			@ApiResponse(responseCode = "200", description = "Policy model retrieved.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("policies")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getPolicies(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request, "GetPolicyModel");
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();

		try (StrolchTransaction tx = rest.openTx(cert, getClass())) {
			ConfigurationPolicy policy = ConfigurationPolicy.getDefaultPolicy(tx);
			PolicyModel policyModel = policy.getPolicyModel();
			return ResponseUtil.toResponse(DATA, policyModelToJson(policyModel));
		}
	}

	@Operation(summary = "Update policies", description = "Updates the policy model.", responses = {
			@ApiResponse(responseCode = "200", description = "Policy model updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Path("policies")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response updatePolicies(@Context HttpServletRequest request, String data) {
		Certificate cert = validateCertificate(request, "UpdatePolicyModel");
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();

		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		try (StrolchTransaction tx = rest.openTx(cert, getClass())) {
			ConfigurationPolicy policy = ConfigurationPolicy.getDefaultPolicy(tx);
			PolicyModel policyModel = policyModelFromJson(jsonObject);
			policy.updatePolicyModel(policyModel);
			tx.commitOnClose();
		}

		return ResponseUtil.toResponse();
	}

	private JsonObject policyModelToJson(PolicyModel policyModel) {
		JsonObject policyTypesJ = new JsonObject();
		for (Map.Entry<String, PolicyType> entry : policyModel.getPolicyTypes().entrySet()) {
			PolicyType policyType = entry.getValue();
			JsonObject policyTypeJ = new JsonObject();
			policyTypeJ.addProperty("type", policyType.getType());
			policyTypeJ.addProperty("api", policyType.getApi());

			JsonObject policyByKeyMapJ = new JsonObject();
			for (Map.Entry<String, String> policyEntry : policyType.getPolicyByKeyMap().entrySet()) {
				policyByKeyMapJ.addProperty(policyEntry.getKey(), policyEntry.getValue());
			}
			policyTypeJ.add("policyByKeyMap", policyByKeyMapJ);

			JsonArray possibleImplementationsJ = new JsonArray();
			for (String implementation : policyType.getPossibleImplementations()) {
				possibleImplementationsJ.add(implementation);
			}
			policyTypeJ.add("possibleImplementations", possibleImplementationsJ);

			policyTypesJ.add(entry.getKey(), policyTypeJ);
		}

		JsonObject resultJ = new JsonObject();
		resultJ.add("policyTypes", policyTypesJ);
		return resultJ;
	}

	private PolicyModel policyModelFromJson(JsonObject jsonObject) {
		PolicyModel policyModel = new PolicyModel();
		JsonObject policyTypesJ = jsonObject.getAsJsonObject("policyTypes");
		for (Map.Entry<String, JsonElement> entry : policyTypesJ.entrySet()) {
			JsonObject policyTypeJ = entry.getValue().getAsJsonObject();
			String type = policyTypeJ.get("type").getAsString();
			String api = policyTypeJ.get("api").getAsString();
			PolicyType policyType = new PolicyType(type, api);

			JsonObject policyByKeyMapJ = policyTypeJ.getAsJsonObject("policyByKeyMap");
			for (Map.Entry<String, JsonElement> policyEntry : policyByKeyMapJ.entrySet()) {
				policyType.getPolicyByKeyMap().put(policyEntry.getKey(), policyEntry.getValue().getAsString());
			}
			policyModel.getPolicyTypes().put(type, policyType);
		}
		return policyModel;
	}
}
