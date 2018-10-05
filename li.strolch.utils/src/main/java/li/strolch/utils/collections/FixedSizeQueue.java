package li.strolch.utils.collections;

import java.util.Collection;
import java.util.LinkedList;

public class FixedSizeQueue<E> extends LinkedList<E> {

	private int maxSize;

	public FixedSizeQueue(int size) {
		this.maxSize = size;
	}

	@Override
	public boolean add(E e) {
		boolean b = super.add(e);
		if (size() >= this.maxSize) {
			removeRange(0, size() - this.maxSize);
		}
		return b;
	}

	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		boolean b = super.addAll(index, c);
		if (size() >= this.maxSize) {
			removeRange(0, size() - this.maxSize);
		}
		return b;
	}
}