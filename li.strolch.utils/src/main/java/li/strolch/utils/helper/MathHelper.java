/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.utils.helper;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * A helper class that contains mathematical computations that can be used throughout.
 *
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 * @author Michael Gatto &lt;michael@gatto.ch&gt;
 */
public class MathHelper {

	public static final double PRECISION = 1.0E08;
	public static final int PRECISION_DIGITS = 3;

	/**
	 * Check if the two values are equal with respect to the precision
	 *
	 * @param firstValue
	 * 		the first value to compare
	 * @param secondValue
	 * 		the second value to compare to
	 *
	 * @return boolean True, if the two values are equal under the set precision. Fales, otherwise.
	 */
	public static boolean isEqualPrecision(double firstValue, double secondValue) {
		return (java.lang.Math.abs(firstValue - secondValue) < (1.0d / PRECISION));
	}

	/**
	 * Comparison between the two values. Given the precision, this function determines if the given value is smaller
	 * than the given bound.
	 * <p>
	 * Note: this implementation tests if the value < bound, and if this is not so, checks if the values are equal under
	 * the precision. Thus, it's efficient whenever the value is expected to be smaller than the bound.
	 *
	 * @param value
	 * 		The value to check
	 * @param bound
	 * 		The bound given
	 *
	 * @return true, if value < bound under the given precision. False, otherwise.
	 */
	public static boolean isSmallerEqualPrecision(double value, double bound) {
		return value < bound || isEqualPrecision(value, bound);
	}

	/**
	 * Comparison between two values. Given the precision, this function determines if the given value is greater than
	 * the given bound.
	 * <p>
	 * Note: This implementation tests if value > bound and, if it is so, if equality does NOT hold. Thus, it is
	 * efficient whenever the value is not expected to be greater than the bound.
	 *
	 * @param value
	 * 		The value to check
	 * @param bound
	 * 		The bound given.
	 *
	 * @return true, if value > bound and the values do not test equal under precision. False, otherwise.
	 */
	public static boolean isGreaterPrecision(double value, double bound) {
		return (value > bound) && !isEqualPrecision(value, bound);
	}

	/**
	 * <p>
	 * Rounds the given double value to the number of decimals
	 * </p>
	 *
	 * <p>
	 * <b>Warning:</b> Do not use the returned value for further calculations. Always finish calculates and use one of
	 * the following methods:
	 * </p>
	 * <ul>
	 * <li>{@link #isEqualPrecision(double, double)},</li>
	 * <li>{@link #isGreaterPrecision(double, double)} or</li>
	 * <li>{@link #isSmallerEqualPrecision(double, double)}</li>
	 * </ul>
	 * to test on equality or greater than/ smaller than
	 *
	 * @param value
	 * 		the double value to round
	 * @param decimals
	 * 		number of decimals
	 *
	 * @return the rounded number
	 */
	public static double toPrecision(double value, int decimals) {
		if (value == 0.0)
			return 0.0;
		if (Double.isNaN(value))
			return Double.NaN;
		if (value == Double.NEGATIVE_INFINITY)
			return Double.NEGATIVE_INFINITY;
		if (value == Double.POSITIVE_INFINITY)
			return Double.POSITIVE_INFINITY;
		return new BigDecimal(value).setScale(decimals, RoundingMode.HALF_EVEN).doubleValue();
	}

	/**
	 * Returns the value with the precision where precision is set to {@link #PRECISION_DIGITS}
	 *
	 * @see #toPrecision(double, int)
	 */
	public static double toPrecision(double value) {
		return toPrecision(value, PRECISION_DIGITS);
	}
}
