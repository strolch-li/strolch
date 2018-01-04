package li.strolch.soql.core;

import java.util.Iterator;

/**
 * Iterator used to build the cartesian product defined in the FROM clause
 */
public class IndexPointer implements Iterator {

    final int[] numberOfEntities;
    final int[] pointer;

    public IndexPointer(int[] numberOfEntities) {
        this.numberOfEntities = numberOfEntities;
        this.pointer = new int[numberOfEntities.length];
        this.pointer[pointer.length-1]--;
    }

    public int[] next() {
        pointer[pointer.length - 1]++;
        normalize();
        return pointer;
    }

    public boolean hasNext() {
        for (int i = 0; i < pointer.length; i++) {
            if (pointer[i] < numberOfEntities[i] - 1) {
                return true;
            }
        }
        return false;
    }

    private void normalize() {
        for (int i = pointer.length - 1; i >= 0; i--) {
            if (pointer[i] == numberOfEntities[i]) {
                pointer[i] = 0;
                pointer[i - 1]++;
            }
        }
    }

}
