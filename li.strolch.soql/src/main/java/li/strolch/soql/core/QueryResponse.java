package li.strolch.soql.core;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.json.StrolchElementToJsonVisitor;

import java.util.*;

/**
 * @author msmock
 */
public class QueryResponse extends QueryRequest {

    // the returned objects
    static final String RESULT_SET = "resultSet";
    public List<StrolchRootElement> resultSet = new ArrayList<>();

    // an exception or error message in case of error
    public String message;

    /**
     * @return the query as JsonObject
     */
    public JsonObject toJson(final StrolchElementToJsonVisitor visitor) {

        final JsonObject rootJ = super.toJson();

        if (message != null && !message.isEmpty()) {
            rootJ.addProperty("Message", message);
        }

        final JsonArray resultSetJ = new JsonArray();
        rootJ.add(RESULT_SET, resultSetJ);

        for (Iterator<StrolchRootElement> iter = resultSet.iterator(); iter.hasNext(); ) {
            resultSetJ.add(iter.next().accept(visitor));
        }

        return rootJ;
    }

}
