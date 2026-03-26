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

package li.strolch.agent.api;

import com.google.gson.JsonObject;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import static java.time.temporal.ChronoUnit.*;

public class CountStatistics {

	private final Queue<CountStatistic> events;

	public CountStatistics() {
		this.events = new ConcurrentLinkedQueue<>();
	}

	public synchronized void recordEvent(int count) {
		LocalDateTime now = LocalDateTime.now();
		this.events.add(new CountStatistic(now, count));
		removeOldEvents(now);
	}

	private void removeOldEvents(LocalDateTime now) {
		while (!this.events.isEmpty() && DAYS.between(this.events.peek().timestamp, now) >= 1) {
			this.events.poll();
		}
	}

	public int getLastMinute() {
		return getInTimeFrame(1, MINUTES);
	}

	public int getLast5Minutes() {
		return getInTimeFrame(5, MINUTES);
	}

	public int getLast15Minutes() {
		return getInTimeFrame(15, MINUTES);
	}

	public int getLastHour() {
		return getInTimeFrame(1, HOURS);
	}

	public int getLastDay() {
		return getInTimeFrame(1, DAYS);
	}

	private synchronized int getInTimeFrame(long amount, ChronoUnit unit) {
		LocalDateTime now = LocalDateTime.now();
		long amountSeconds = unit.getDuration().getSeconds() * amount;
		return (int) this.events.stream().filter(tx -> SECONDS.between(tx.timestamp, now) < amountSeconds).count();
	}

	public synchronized JsonObject toJson() {
		List<Integer> counts = this.events.stream().map(e -> e.maxCount).toList();

		long count = counts.size();
		long sum = counts.stream().mapToLong(Integer::longValue).sum();
		int avg = count == 0 ? 0 : (int) ((double) sum / count);
		int median = getMedian(counts);

		int min = counts.stream().mapToInt(Integer::intValue).min().orElse(0);
		int max = counts.stream().mapToInt(Integer::intValue).max().orElse(0);

		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty("lastMinute", getLastMinute());
		jsonObject.addProperty("last5Minutes", getLast5Minutes());
		jsonObject.addProperty("last15Minutes", getLast15Minutes());
		jsonObject.addProperty("lastHour", getLastHour());
		jsonObject.addProperty("lastDay", getLastDay());
		jsonObject.addProperty("minCount", min);
		jsonObject.addProperty("maxCount", max);
		jsonObject.addProperty("avgCount", avg);
		jsonObject.addProperty("medianCount", median);
		jsonObject.addProperty("totalCount", sum);

		return jsonObject;
	}

	private int getMedian(List<Integer> counts) {
		if (counts.isEmpty())
			return 0;
		List<Integer> sorted = counts.stream().sorted().toList();
		int middle = sorted.size() / 2;
		if (sorted.size() % 2 == 0) {
			return (sorted.get(middle - 1) + sorted.get(middle)) / 2;
		} else {
			return sorted.get(middle);
		}
	}

	private static class CountStatistic {
		LocalDateTime timestamp;
		int maxCount;

		CountStatistic(LocalDateTime timestamp, int maxCount) {
			this.timestamp = timestamp;
			this.maxCount = maxCount;
		}
	}
}