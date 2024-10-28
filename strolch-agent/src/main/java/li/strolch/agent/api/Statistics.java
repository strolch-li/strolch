package li.strolch.agent.api;

import com.google.gson.JsonObject;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import static java.time.temporal.ChronoUnit.*;

public class Statistics {

	private final Queue<Statistic> events;

	public Statistics() {
		this.events = new ConcurrentLinkedQueue<>();
	}

	public synchronized void recordEvent(Duration duration) {
		LocalDateTime now = LocalDateTime.now();
		this.events.add(new Statistic(now, duration));
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
		List<Duration> durations = this.events.stream().map(e -> e.duration).toList();

		long count = durations.size();
		long sum = durations.stream().mapToLong(Duration::toMillis).sum();
		int avg = count == 0 ? 0 : (int) ((double) sum / count);

		Duration min = durations.stream().min(Duration::compareTo).orElse(Duration.ZERO);
		Duration max = durations.stream().max(Duration::compareTo).orElse(Duration.ZERO);
		Duration median = getMedian(durations);

		JsonObject jsonObject = new JsonObject();
		jsonObject.addProperty("lastMinute", getLastMinute());
		jsonObject.addProperty("last5Minutes", getLast5Minutes());
		jsonObject.addProperty("last15Minutes", getLast15Minutes());
		jsonObject.addProperty("lastHour", getLastHour());
		jsonObject.addProperty("lastDay", getLastDay());
		jsonObject.addProperty("minDuration", min.toMillis());
		jsonObject.addProperty("maxDuration", max.toMillis());
		jsonObject.addProperty("avgDuration", avg);
		jsonObject.addProperty("medianDuration", median.toMillis());

		return jsonObject;
	}

	private Duration getMedian(List<Duration> durations) {
		if (durations.isEmpty())
			return Duration.ZERO;
		List<Duration> sorted = durations.stream().sorted().toList();
		int middle = sorted.size() / 2;
		if (sorted.size() % 2 == 0) {
			return sorted.get(middle - 1).plus(sorted.get(middle)).dividedBy(2);
		} else {
			return sorted.get(middle);
		}
	}

	private static class Statistic {
		LocalDateTime timestamp;
		Duration duration;

		Statistic(LocalDateTime timestamp, Duration duration) {
			this.timestamp = timestamp;
			this.duration = duration;
		}
	}
}