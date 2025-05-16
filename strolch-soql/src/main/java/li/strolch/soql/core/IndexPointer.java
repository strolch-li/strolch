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

package li.strolch.soql.core;

import java.util.Iterator;

/**
 * Iterator used to build the cartesian product defined in the FROM clause
 */
public class IndexPointer implements Iterator<int[]> {

	final int[] numberOfEntities;
	final int[] pointer;

	public IndexPointer(int[] numberOfEntities) {
		this.numberOfEntities = numberOfEntities;
		this.pointer = new int[numberOfEntities.length];
		this.pointer[pointer.length - 1]--;
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
