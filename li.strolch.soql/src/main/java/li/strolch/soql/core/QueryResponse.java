package li.strolch.soql.core;

import com.google.gson.JsonObject;

/**
 * @author msmock
 */
public class QueryResponse extends QueryRequest {

    // the returned objects
    static final String RESULT_SET = "resultSet";

    // an exception or error message in case of error
    public String message;

    //
    public final ResultSet resultSet = new ResultSet();

    /**
     * @return the query as JsonObject
     */
    public JsonObject asJson() {

        final JsonObject rootJ = super.asJson();

        if (message != null && !message.isEmpty()) {
            rootJ.addProperty("Message", message);
        }

        rootJ.add(RESULT_SET, resultSet.asJson());

        return rootJ;
    }

}
