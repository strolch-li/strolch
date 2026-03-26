package li.strolch.agent.api;

import com.google.gson.JsonObject;
import org.junit.Test;

import java.time.Duration;

import static org.junit.Assert.assertEquals;

public class DurationStatisticsTest {

	@Test
	public void shouldRecordEvents() {
		DurationStatistics stats = new DurationStatistics();
		stats.recordEvent(Duration.ofMillis(100));
		stats.recordEvent(Duration.ofMillis(200));
		stats.recordEvent(Duration.ofMillis(300));

		JsonObject json = stats.toJson();
		assertEquals(3, json.get("lastMinute").getAsInt());
		assertEquals(3, json.get("last5Minutes").getAsInt());
		assertEquals(3, json.get("last15Minutes").getAsInt());
		assertEquals(3, json.get("lastHour").getAsInt());
		assertEquals(3, json.get("lastDay").getAsInt());
		assertEquals(100, json.get("minDuration").getAsLong());
		assertEquals(300, json.get("maxDuration").getAsLong());
		assertEquals(200, json.get("avgDuration").getAsLong());
		assertEquals(200, json.get("medianDuration").getAsLong());
	}

	@Test
	public void shouldCalculateMedian() {
		DurationStatistics stats = new DurationStatistics();
		stats.recordEvent(Duration.ofMillis(10));
		stats.recordEvent(Duration.ofMillis(20));
		stats.recordEvent(Duration.ofMillis(30));
		stats.recordEvent(Duration.ofMillis(40));

		JsonObject json = stats.toJson();
		// (20 + 30) / 2 = 25
		assertEquals(25, json.get("medianDuration").getAsLong());

		stats.recordEvent(Duration.ofMillis(50));
		json = stats.toJson();
		// 10, 20, 30, 40, 50 -> 30
		assertEquals(30, json.get("medianDuration").getAsLong());
	}

	@Test
	public void shouldHandleEmptyStats() {
		DurationStatistics stats = new DurationStatistics();
		JsonObject json = stats.toJson();
		assertEquals(0, json.get("lastMinute").getAsInt());
		assertEquals(0, json.get("minDuration").getAsLong());
		assertEquals(0, json.get("maxDuration").getAsLong());
		assertEquals(0, json.get("avgDuration").getAsLong());
		assertEquals(0, json.get("medianDuration").getAsLong());
	}
}
