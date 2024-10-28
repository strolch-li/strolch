package li.strolch.utils;

public class RoundRobinInt {
	private final int start;
	private final int stop;
	private int index;

	public RoundRobinInt(int start, int stop) {
		if (start >= stop)
			throw new IllegalArgumentException(
					"A round robin start >= stop does not make sense: " + start + " >= " + stop);
		this.start = start;
		this.stop = stop;
		this.index = start;
	}

	public int next() {
		int next = this.index++;
		if (this.index > this.stop)
			this.index = this.start;
		return next;
	}

	public void toStart() {
		this.index = this.start;
	}
}
