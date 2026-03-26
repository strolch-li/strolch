package li.strolch.agent.api;

import com.google.gson.JsonObject;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CountStatisticsTest {

	@Test
	public void shouldRecordEvents() {
		CountStatistics stats = new CountStatistics();
		stats.recordEvent(1);
		stats.recordEvent(2);
		stats.recordEvent(3);

		JsonObject json = stats.toJson();
		assertEquals(3, json.get("lastMinute").getAsInt());
		assertEquals(3, json.get("last5Minutes").getAsInt());
		assertEquals(3, json.get("last15Minutes").getAsInt());
		assertEquals(3, json.get("lastHour").getAsInt());
		assertEquals(3, json.get("lastDay").getAsInt());
		assertEquals(1, json.get("minCount").getAsInt());
		assertEquals(3, json.get("maxCount").getAsInt());
		assertEquals(2, json.get("avgCount").getAsInt());
		assertEquals(2, json.get("medianCount").getAsInt());
		assertEquals(6, json.get("totalCount").getAsLong());
	}

	@Test
	public void shouldCalculateMedian() {
		CountStatistics stats = new CountStatistics();
		stats.recordEvent(10);
		stats.recordEvent(20);
		stats.recordEvent(30);
		stats.recordEvent(40);

		JsonObject json = stats.toJson();
		// (20 + 30) / 2 = 25
		assertEquals(25, json.get("medianCount").getAsInt());

		stats.recordEvent(50);
		json = stats.toJson();
		// 10, 20, 30, 40, 50 -> 30
		assertEquals(30, json.get("medianCount").getAsInt());
	}

	@Test
	public void shouldHandleEmptyStats() {
		CountStatistics stats = new CountStatistics();
		JsonObject json = stats.toJson();
		assertEquals(0, json.get("lastMinute").getAsInt());
		assertEquals(0, json.get("minCount").getAsInt());
		assertEquals(0, json.get("maxCount").getAsInt());
		assertEquals(0, json.get("avgCount").getAsInt());
		assertEquals(0, json.get("medianCount").getAsInt());
		assertEquals(0, json.get("totalCount").getAsLong());
	}
}
