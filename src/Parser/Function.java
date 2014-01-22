package Parser;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import File.Cell;
import File.Sheet;
import File.Sheet.Position;
import File.Sheet.Range;

/**
 * Enumeration that binds functions to their function name.<br>
 * Functions must implement a <code>calculate</code> method, which takes one or
 * more (arg, varargs) <code>Object</code> parameters of type
 * <code>Number</code>, <code>Boolean</code>, <code>Reference</code> or
 * <code>Range</code> and returns the result of the calculation as
 * <code>Number</code>.
 * 
 * @author Jan-Willem Gmelig Meyling
 * @author Maarten Flikkema
 */
public enum Function {
	
	/**
	 * <div> <b>Expected arguments:</b> <code>value</code>, <code>[value]...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The sum of all the arguments</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The <code>SUM</code>-function adds the values of the arguments, and
	 * returns the result in the type of the first argument. For example: if the
	 * first argument is an instance of <code>Integer</code> and the second and
	 * third arguments are instances of <code>Double</code>, these arguments are
	 * rounded to the closest <code>Integer</code> and added to the result of
	 * type <code>Integer</code>.<br>
	 * When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SUM("Returns the sum of a set of values contained in a specified field on a query.", "<b>value</b>, value, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			double output = 0;
			
			List<Object> values = new ArrayList<Object>(arguments.length);
			values.addAll(Arrays.asList(arguments));
						
			for (Object argument : arguments) {
				if (argument instanceof Range) {
					Range range = (Range) argument;
					values.addAll(Arrays.asList(range.getValueArray()));
					values.remove(range);
				}
			}
			
			for (Object value : values ) {
				try {
					output += doubleValueOf(value);
				} catch (NumberFormatException e) {
					// Ignore not parsable Strings
				}
			}
			
			return convertToIntIfApplicable(output);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>starting number</code>, <code>subtr. number</code>, <code>[subtr. numbers...]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Result of the subtraction in the type of the <code>starting number</code>.</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The <code>SUBTRACT</code>-function subtracts the values of the arguments
	 * from the first argument and returns the result in the type of the first
	 * argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second and third arguments are instances of
	 * <code>Double</code>, these arguments are rounded to the closest
	 * <code>Integer</code>.<br>
	 * Booleans (TRUE/FALSE) are interpreted here as resp. 1 and 0.<br>
	 * When the input doesn't make any sense (eg. adding a <code>String</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SUBTRACT("Subtracts the first argument by al the following.", "<b>starting value, subtract value</b>, subtract value, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(2, arguments.length);
			double output = doubleValueOf(arguments[0]);
			for (int i = 1; i < arguments.length; i++ ) {
				output -= doubleValueOf(arguments[i]);
			}
			return convertToIntIfApplicable(output);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>starting number</code>, <code>mult. number</code>, <code>[mult. numbers...]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>See comments</li>
	 * </ul>
	 * </div>
	 * The <code>PRODUCT</code>-function multiplies the values of the
	 * arguments, and returns the result in the type of the first argument. For
	 * example: if the first argument is an instance of <code>Integer</code> and
	 * the second and third arguments are instances of <code>Double</code>,
	 * these arguments are rounded to the closest <code>Integer</code>.<br/>
	 * When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	PRODUCT("Multiplies all the numbers given as arguments and returns the product.", "<b>starting value, mult. value</b>, mult.value, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(2, arguments.length);
			double output = doubleValueOf(arguments[0]);
			for (int i = 1; i < arguments.length; i++) {
				output *= doubleValueOf(arguments[i]);
			}
			return convertToIntIfApplicable(output);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>, <code>divider</code>, <code>[divider]...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>See comments</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The <code>DIVIDE</code>-function divides the values of the arguments from
	 * the first argument, and returns the result in the type of the first
	 * argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second and third arguments are instances of
	 * <code>Double</code>, these arguments are rounded to the closest
	 * <code>Integer</code>. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	DIVIDE("Divides the first argument by the second. If more arguments are given, the value is again divided by the next argument and so on.", "<b>value, denominator</b>, denominator, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(2, arguments.length);
			double output = doubleValueOf(arguments[0]);
			for (int i = 1; i < arguments.length; i++) {
				assertArgumentSingleRange(arguments[i]);
				output /= doubleValueOf(arguments[i]);
			}
			return convertToIntIfApplicable(output);
		}
	},
	
	/**
	 * <div>
	 * <b>Arguments:</b> <code>base</code>, <code>power</code>
	 * </div>
	 * <div>
	 * The <code>POWER</code>-function takes two arguments, and returns the
	 * first argument to the power of the second argument, in the type of the
	 * first argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second argument is an instances of
	 * <code>Double</code>, this argument rounded to the closest
	 * <code>Integer</code> for the calculation. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div>
	 * <div>
	 * <b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	POWER("Returns the result of a number raised to a power.", "<b>base, power</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			return convertToIntIfApplicable(Math.pow(doubleValueOf(arguments[0]), doubleValueOf(arguments[1])));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>, <code>denominator</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	MOD("Returns the modulo of the division of the first argument by the second argument.", "<b>value, denominator</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			return convertToIntIfApplicable(doubleValueOf(arguments[0]) % doubleValueOf(arguments[1]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code> As Double, <code>[real number...]</code> As Double
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Average of all the numerical values in the given range(s).</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	AVERAGE("Returns the average (arithmetic mean) of the arguments.", "<b>value(s)</b>, value(s), ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			return convertToIntIfApplicable(doubleValueOf(SUM.calculate(arguments)) / doubleValueOf(COUNT.calculate(arguments)));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code> As Double, <code>[real number...]</code> As Double
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Average of all the numerical values in the given range(s).</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	MEDIAN("Returns the median of the arguments", "<b>value(s)</b>, value(s), ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			
			ArrayList<Double> countNumbersList = new ArrayList<Double>();
			
			for(Object arg : arguments) {
				if (arg instanceof Range) {
					for (Cell cell : ((Range) arg).getCellArray()) {
						countNumbersList.add(doubleValueOf(cell));
					}
				} else {
					countNumbersList.add(doubleValueOf(arg));
				}
			}
			
			double[] numbersList = new double[countNumbersList.size()];
			
			for(int i = 0; i < numbersList.length; i++) {
				numbersList[i] = countNumbersList.get(i);
			}
			
			Arrays.sort(numbersList);
			
			double median;
			
			if (numbersList.length % 2 == 0) {
				median = ((double) numbersList[numbersList.length / 2 - 1] + (double) numbersList[numbersList.length / 2]) / 2;
			} else {
				median = (double) numbersList[(int)Math.floor(numbersList.length / 2.0)];
			}
			
			return convertToIntIfApplicable(median);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>range</code>, <code>[range...]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Number of cells in the given ranges with a numerical value in it</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	COUNT("Returns the number of numerical values of the arguments.", "<b>range</b>, range, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			int count = 0;
			for(Object arg : arguments) {
				assertArgumentRange(arg);	// assert arg instanceof Range : "Argument type error! All arguments in this function must be a Range.";
				Range rng = (Range)arg;
				for (Cell cell : rng.getCellArray()) {
					if ((Boolean) ISNUMBER.calculate(cell)) {
						count++;
					}
				}	
			}
			return count;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>range</code>, <code>[range...]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Number of cells in the given range(s) that are not empty</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	COUNTA("Returns the number of not empty cells in the arguments.", "<b>range</b>, range, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			int count = 0;
			for(Object arg : arguments) {
				assertArgumentRange(arg);	// assert arg instanceof Range : "Argument type error! All arguments in this function must be a Range.";
				Range rng = (Range)arg;
				for (Cell cell : rng.getCellArray()) {
					if (!cell.getValue().equals("")) {
						count++;
					}
				}	
			}
			return count;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>[argument]</code>, <code>[oneindig argument]...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	COUNTIF("Counts the number of cells within a range that meet the given criteria.", "<b>range, criteria</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			assertArgumentRange(0, arguments);
			
			int count = 0;
			Cell[] range = ((Range) arguments[0]).getCellArray();
			String criteria = arguments[1].toString();
			
			if ("<>!=".indexOf(criteria.charAt(0)) == -1) {
				criteria = "==".concat(criteria);	// geen operator -> ==
			} else if ( criteria.charAt(0) == '=' & criteria.charAt(1) != '=' ) {
				criteria = "=".concat(criteria);	// = -> ==
			}
			
			for ( int i = 0; i < range.length; i++ ) {
				Cell cell = range[i];
				if ( cell == null )
					continue;
				if (new Parser(cell.getSheet(), cell.getPositionString()
						+ criteria).parse().equals(Boolean.TRUE)) {
					count++;
				}
			}
			return count;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>range</code>, <code>criteria</code>, [<code>sum range</code>]
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>If <code>sum range</code> is supplied: sum of all the values in <code>sum range</code> for which the same index in <code>range</code> satisfies <code>criteria</code></li>
	 * <li>Else: sum of all the values in <code>range</code> which satisfy <code>criteria</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	SUMIF("Adds the cells specified by a given criteria.", "<b>range, criteria</b>, sum range") {
		@Override
		public Object calculate(Object... arguments) {
			assertTwoArguments(2, 3, arguments.length);
			assertArgumentRange(0, arguments);
			
			if (arguments.length == 3) {
				assertArgumentRange(2, arguments);
			}
			
			double sum = 0;
			Cell[] range = ((Range) arguments[0]).getCellArray();
			Cell[] sumRange = (arguments.length == 3 && arguments[2] instanceof Range) ? ((Range) arguments[2]).getCellArray() : range;
			String criteria = arguments[1].toString();
			
			if (sumRange.length < range.length) {
				throw new IllegalArgumentException("The sum range must have at least the size of the condition range.");
			}
			
			if ("<>!=".indexOf(criteria.charAt(0)) == -1) {
				criteria = "==".concat(criteria);	// geen operator -> ==
			} else if ( criteria.charAt(0) == '=' & criteria.charAt(1) != '=' ) {
				criteria = "=".concat(criteria);	// = -> ==
			}

			for (int i = 0; i < range.length; i++) {
				Cell cell = range[i];
				Cell valueCell = sumRange[i];
				if (cell == null || valueCell == null) {
					continue;
				}
				if (new Parser(cell.getSheet(), cell.getPositionString()
						+ criteria).parse().equals(Boolean.TRUE)) {
					try {
						sum += doubleValueOf(valueCell);
					} catch ( NumberFormatException e ) {}
				}
			}
			return convertToIntIfApplicable(sum);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li><code>TRUE</code> if <code>argument</code> is a numerical value</li>
	 * <li><code>FALSE</code> if <code>argument</code> is not a numerical value</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ISNUMBER("Returns the logical value TRUE if value is a number; otherwise, it returns FALSE.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			if (arguments[0] instanceof Range) {
				return ISNUMBER.calculate(((Range) arguments[0]).firstCell());
			} else if (arguments[0] instanceof Cell) {
				return ISNUMBER.calculate(((Cell) arguments[0]).getValue());
			}
			return (arguments[0] instanceof Number);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>TRUE if <code>value</code> is an even number</li>
	 * <li>FALSE if <code>value</code> is an odd number</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If the value is not integer, it is truncated
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ISEVEN("Returns true if the (truncated) value is an even number; otherwise, it returns FALSE.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return (intValueOf(arguments[0]) % 2 == 0);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>TRUE if <code>value</code> is an odd number</li>
	 * <li>FALSE if <code>value</code> is an even number</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If the value is not integer, it is truncated (like the INT function).
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ISODD("Returns true if the (truncated) value is an odd number; otherwise, it returns FALSE.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return !(intValueOf(arguments[0]) % 2 == 0);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code> As Double, <code>decimals places</code> As Integer
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Mathematicaly correct rounded value of <code>number</code> to <code>decimals places</code> decimal places</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ROUND("The ROUND function rounds a number to a specified number of digits.", "<b>value</b>, num digits") {
		@Override
		public Object calculate(Object... arguments) {
			assertTwoArguments(1, 2, arguments.length);
			double value = doubleValueOf(arguments[0]);
			if (arguments.length == 1 || intValueOf(arguments[1]) == 0) {
				return (int) Math.round(value);
			} else {
				int decPlaces = intValueOf(arguments[1]);
				return ((double) Math.floor((Math.pow(10, decPlaces) * value) + 0.5d)) / Math.pow(10, decPlaces);
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>number</code> As Double, <code>num_digits</code> As Integer
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ROUNDDOWN("Rounds a number down, toward zero, at a given number of digits.", "<b>value</b>, num digits") {
		@Override
		public Object calculate(Object... arguments) {
			assertTwoArguments(1, 2, arguments.length);
			
			int sign = intValueOf(SIGN.calculate(arguments[0]));
			double value = Math.abs(doubleValueOf(arguments[0]));
			int decPlaces = 0;
			
			if (arguments.length == 2) {
				decPlaces = intValueOf(arguments[1]);
			}
			
			value = ((double) Math.floor((Math.pow(10, decPlaces) * value))) / Math.pow(10, decPlaces);
			
			return convertToIntIfApplicable(sign * value);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>number</code> As Double, [<code>num_digits</code>] As Integer
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ROUNDUP("Rounds a number up, away from zero, at a given number of digits.", "<b>value</b>, num digits") {
		@Override
		public Object calculate(Object... arguments) {
			assertTwoArguments(1, 2, arguments.length);
			
			int sign = intValueOf(SIGN.calculate(arguments[0]));
			double value = Math.abs(doubleValueOf(arguments[0]));
			int decPlaces = 0;
			
			if (arguments.length == 2) {
				decPlaces = intValueOf(arguments[1]);
			}
			
			if (Math.round(value * Math.pow(10, decPlaces)) != (value * Math.pow(10, decPlaces))) {
				value = ((double) Math.floor((Math.pow(10, decPlaces) * value)) + 1d) / Math.pow(10, decPlaces);
			} else {
				value = ((double) Math.floor((Math.pow(10, decPlaces) * value))) / Math.pow(10, decPlaces);
			}
			
			return convertToIntIfApplicable(sign * value);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul><li>The integer portion of a number</li></ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * Different than ROUNDDOWN! INT will just cut of the decimal part of <code>value</code>
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	INT("Returns the integer part/portion of a number.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return (int) Math.floor(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>string</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul><li>The given string in lower case</li></ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * ...
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	LOWER("Converts the input to lower case", "<b>string</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return stringValueOf(arguments[0]).toLowerCase(); 
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>string</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul><li>The given string with all characters following a not-letter upper case and all others lower case.</li></ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The first character will always be converted to uppercase.
	 * All letters [a-z,A-Z] following a non-letter wil be converted to upper case.
	 * All letters following a letter wil be converted to lower case.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	PROPER("Capitalizes the first letter in a text string and any other letters in text that follow any character other than a letter. Converts all other letters to lowercase letters.", "<b>string</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			
			String strIn = stringValueOf(arguments[0]);
			char[] result = new char[strIn.length()];
			result[0] = Character.toUpperCase(strIn.charAt(0));

			for (int i = 1; i < result.length; i++) {
				char temp = strIn.charAt(i);
				
				if (Character.isLetter(strIn.charAt(i - 1))) {
					result[i] = Character.toLowerCase(temp);
				} else {
					result[i] = Character.toUpperCase(temp);
				}
			}
			return new String(result);
		}
		
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>string</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul><li>The given string in upper case</li></ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * ...
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	UPPER("Converts the input to upper case", "<b>string</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return stringValueOf(arguments[0]).toUpperCase(); 
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	RAW() {
		@Override
		public Object calculate(Object... arguments) {
			return Arrays.toString(arguments);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <i>None</i>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>A random <code>Double</code> number between 0 and 1.</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	RAND("Returns an evenly distributed random number greater than or equal to 0 and less than 1. A new random number is returned every time the sheet is recalculated.", "<i>none</i>") {
		@Override
		public Object calculate(Object... arguments) {
			return Math.random();
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>lower limit</code> As Integer, <code>upper limit</code> As Integer
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>A random Integer number between <code>lower limit</code> and <code>upper limit</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	RANDBETWEEN("Returns a random integer between the numbers you specify. A new random number is returned every time the sheet is calculated.", "<b>lower limit, upper limit</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			int a = intValueOf(arguments[0]);
			int b = intValueOf(arguments[1]);
			return (int) (Math.random() * (b - a + 1) + a);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Square root of <code>value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SQRT("Returns the square root.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			double output = Math.sqrt(doubleValueOf(arguments[0]));
			return convertToIntIfApplicable(output);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>, <code>root-base</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The <code>root-base</code> root of <code>value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ROOT("Returns the second argument-base root of the first argument.", "<b>value, root-base</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			return convertToIntIfApplicable(Math.pow(doubleValueOf(arguments[0]), 1.0 / doubleValueOf(arguments[1])));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The sin of <code>real number</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SIN("Returns a Double specifying the sine of an angle.", "<b>angle</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.sin(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ASIN("Returns the arcsine, or inverse sine, of a number. The arcsine is the angle whose sine is number. The returned angle is given in radians in the range -pi/2 to pi/2.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.asin(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ACOS("Returns the arccosine, or inverse cosine, of a number. The arccosine is the angle whose cosine is number. The returned angle is given in radians in the range 0 (zero) to pi.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.acos(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ATAN("Returns the arctangent, or inverse tangent, of a number. The arctangent is the angle whose tangent is number. The returned angle is given in radians in the range -pi/2 to pi/2.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.atan(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>angle</code> (radians)
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Angle converted to degrees</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	DEGREE("Converts radians to degrees.", "<b>radian angle</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.toDegrees(doubleValueOf(arguments[0]));
		}
	},

	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>angle</code> (degrees)
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Angle converted to radians</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	RADIAN("Converts degrees to radians.", "<b>degree angle</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.toRadians(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The cosine of <code>real number</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * Get the cos of a value. Returns a <code>Double</code> value.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	COS("Returns a Double specifying the cosine of an angle.", "<b>radian angle</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.cos(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The tan of <code>real number</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	TAN("Returns a Double specifying the tangent of an angle.", "<b>radian angle</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return Math.tan(doubleValueOf(arguments[0]));			
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> none
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The value of Pi (3,1415...) to 15 decimal places</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	PI("Returns a Double value of pi.", "<i>none</i>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(0, arguments.length);
			return Math.PI;		
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>[power value]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The value of e to the power of <code>real power value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If the power-arguments is not supplied, <code>EXP</code> will return the value of e (to the power 1).
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	EXP("Returns a Double value of e to the power of the given argument.", "power value") {
		@Override
		public Object calculate(Object... arguments) {
			assertTwoArguments(0, 1, arguments.length);
			double power = 1;
			if(arguments.length > 0) {
				power = doubleValueOf(arguments[0]);
			}
			return convertToIntIfApplicable(Math.exp(power));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The base-10 log of <code>value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	LOG("Returns the logarithm of a number to base 10.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return convertToIntIfApplicable(Math.log10(doubleValueOf(arguments[0])));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>, <code>base value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The <code>base value</code>-base log of <code>value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	LOGBASE("Returns the logarithm of a number to the base you specify.", "<b>value, log base</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			return convertToIntIfApplicable(Math.log10(doubleValueOf(arguments[0])) / Math.log10(intValueOf(arguments[1])));			
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The natural logarithm of a number.</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	LN("Returns a Double specifying the natural logarithm of a number.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return convertToIntIfApplicable(Math.log(doubleValueOf(arguments[0])));			
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>If the argument is positive, the <code>SIGN</code> function will return <code>1</code></li>
	 * <li>If the argument is negative, the <code>SIGN</code> function will return <code>-1</code></li>
	 * <li>If the argument is zero, the <code>SIGN</code> function will return <code>0</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	SIGN("Determines the sign of a number. Returns 1 if the number is positive, zero (0) if the number is 0, and -1 if the number is negative.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			if (doubleValueOf(arguments[0]) < 0) {
				return -1;
			} else if (doubleValueOf(arguments[0]) > 0) {
				return 1;
			}
			return 0;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The absolute value of the argument</li>
	 * </ul>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ABS("Returns a value of the same type that is passed to it specifying the absolute value of a number.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return convertToIntIfApplicable(Math.abs(doubleValueOf(arguments[0])));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>, <code>[value]...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The lowest value all the values supplied in the arguments</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If there are no number values in the arguments, 0 is returned.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	MIN("Return the minimum of a set of values contained in a specified field on a query.", "<b>value(s)</b>, value(s), ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			boolean aValue = false;
			double min = Double.MAX_VALUE;
			
			for (Object arg : arguments) {
				if (arg instanceof String) {
					continue;
				} else if (arg instanceof Range) {
					for (Cell cell : ((Range) arg).getCellArray()) {
						if (cell.getValue() instanceof String) continue;
						double d = doubleValueOf(cell);
						if (d < min) {
							aValue = true;
							min = d;
						}
					}
				} else if (arg instanceof Cell && ((Cell) arg).getValue() instanceof String) {
					continue;
				} else {
					double d = doubleValueOf(arg);
					if (d < min) {
						aValue = true;
						min = d;
					}
				}
			}
			return aValue ? convertToIntIfApplicable(min) : 0;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value, [value], ...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The greatest value all the values supplied in the arguments</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If there are no number values in the arguments, 0 is returned.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	MAX("Return the maximum of a set of values contained in a specified field on a query.", "<b>value(s)</b>, value(s), ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			int counter = 0;
			double max = 0;
			
			for (Object arg : arguments) {
				if (arg instanceof String) {
					continue;
				} else if (arg instanceof Range) {
					for (Cell cell : ((Range) arg).getCellArray()) {
						if (cell.getValue() instanceof String) continue;
						double d = doubleValueOf(cell);
						if (counter == 0 || d > max) {
							max = d;
						}
						counter++;
					}
				} else if (arg instanceof Cell && ((Cell) arg).getValue() instanceof String) {
					continue;
				} else {
					double d = doubleValueOf(arg);
					if (counter == 0 || d > max) {
						max = d;
					}
				}
				counter++;
			}
			return convertToIntIfApplicable(max);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>logical test, [value if true, [value if false]]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li><code>argument[1]</code> (<code>value if true</code>) if <code>argument[0]</code> (<code>logical test</code>) is true</li>
	 * <li><code>argument[2]</code> (<code>value if false</code>)</li> if <code>logical test</code> is false
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If "<code>value if true</code>" is missing, <code>TRUE</code> will be shown if <code>logical test</code> is true.<br>
	 * If "<code>value if false</code>" is missing, <code>FALSE</code> will be shown if the <code>logical test</code> is false.
	 * It is not possible to imply <code>value if false</code> and not imply <code>value if true</code>.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul><li>Maarten Flikkema</li></ul>
	 * </div>
	 */
	IF("Returns the second argument is the first argument is true, else returns the third argument.", "<b>logical test</b>, value if true, value if false") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			assertMaxArguments(3, arguments.length);
			if (booleanValueOf(arguments[0])) {
				if (arguments.length >= 2) {
					return arguments[1];
				} else {
					return true;
				}
			} else {
				if (arguments.length >= 3) {
					return arguments[2];
				} else {
					return false;
				}
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>logical value</code>, [<code>logical values...</code>]
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li><code>TRUE</code> if one or more logical values are true</li>
	 * <li><code>FALSE</code> if none of the logical values are true</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	OR("Returns TRUE if any argument is TRUE; returns FALSE if all arguments are FALSE.", "<b>logical value</b>, logical value, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			for(Object argument : arguments) {
				if (booleanValueOf(argument) == true) {
					return true;
				}
			}
			return false;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>logical value</code>, [<code>logical values...</code>]
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li><code>TRUE</code> if all logical values are true</li>
	 * <li><code>FALSE</code> if none of the logical values are true</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	AND("Returns TRUE if all its arguments evaluate to TRUE; returns FALSE if one or more arguments evaluate to FALSE.", "<b>logical value</b>, logical value, ...") {
		@Override
		public Object calculate(Object... arguments) {
			assertMinArguments(1, arguments.length);
			for(Object argument : arguments) {
				if (!booleanValueOf(argument)) {
					return false;
				}
			}
			return true;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>logical value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>TRUE if the <code>logical value</code> is FALSE, and vice versa</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	NOT("Reverses the value of its argument. Use NOT when you want to make sure a value is not equal to one particular value.", "<b>logical value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return !booleanValueOf(arguments[0]);
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>TRUE if the value is instance of Boolean</li>
	 * <li>FALSE if the value is not instance of Boolean</li>
	 * </ul>
	 * </div> <div><b>Comments:</b><br>
	 * If more arguments are supplied, or a range containing more than one cell,
	 * all the cells are checked for logical values and <code>true</code> is
	 * only returned if all cells got a logical value.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	ISLOGICAL("Returns true if the given argument is a logical value (TRUE/FALSE).", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			Object arg = arguments[0];
			//assertArgumentSingleRange(arg);
			
			if (arg instanceof Boolean) {
				return true;
			} else if (arg instanceof Cell) {
				return (((Cell)arg).getValue() instanceof Boolean);
			} else if (arg instanceof Range) {
				for (Cell cell : ((Range)arg).getCellArray()) {
					if (ISLOGICAL.calculate(cell).equals(Boolean.FALSE)) {
						return false;
					}
				}
				return true;
			} else {
				return false;
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>format string</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Date</li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	DATE("", "<b>format string</b>") {
		@SuppressWarnings("deprecation")
		@Override
		public Object calculate(Object... arguments) {
			String format = arguments.length == 4 && arguments[3] instanceof String ? (String) arguments[1] : "YYYY-MM-DD";
			return new SimpleDateFormat(format).format(new Date(intValueOf(arguments[0]) - 1900, intValueOf(arguments[1]), intValueOf(arguments[2])));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>range</code>, <code>row_number</code>, <code>column_number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Returns the value of the cell <code>row_number</code> rows down and <code>column_number</code> columns right,
	 * relative to the given range.</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If row or column number is greater then the size of the range, a <code>#VALUE</code> error is returned.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	INDEX("Returns the value of the cell at the given row and column in a given table range.", "<b>table range, row num, column num</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(3, arguments.length);
			assertArgumentRange(0, arguments);
			
			Range matrix = ((Range) arguments[0]);
			int column = intValueOf(arguments[2]) - 1;
			int row = intValueOf(arguments[1]) - 1;

			Position position = matrix.getTopLeft().offset(column, row);
			
			if (!matrix.contains(position)) {
				throw new IllegalArgumentException("The cell at the given position (" + position.toString() + ") does not intersect with the given table range");
			} else {
				return matrix.getSheet().getCellAt(position).getValue();
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>search value</code>, <code>table range</code>, <code>column_num</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The value of the cell <code>column_num</code> to the right of the cell in the first column of <code>table range</code> which is equal to <code>search value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * If the column offset is greater then the width of the range, a <code>#VALUE</code> error is returned.
	 * If the <code>search value</code> is not found in the first column of the <code>table range</code>, a <code>#VALUE</code> error is returned.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	VLOOKUP("Searches for a value in the leftmost column of a range and returns the value of the cell in the same row from a column in the range you specified.", "<b>search value, table range, column num</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(3, arguments.length);
			assertArgumentRange(1, arguments);
			
			String searchVal = stringValueOf(arguments[0]);
			Range matrix = ((Range)arguments[1]);
			int width = matrix.getColumnCount();
			int columnNum = intValueOf(arguments[2]);
			
			if(columnNum < 1 || columnNum > width) {
				throw new IllegalArgumentException("Column number " + columnNum + " does not exist in the given table range.");
			} else {
				int firstColumn = matrix.firstCell().getColumn();
				for (Cell cell : matrix.getCellArray()) {
					if(cell.getColumn() == firstColumn) {
						if(stringValueOf(cell.getValue()).equals(searchVal)) {
							return INDEX.calculate(matrix, cell.getRow() - matrix.firstCell().getRow() + 1, columnNum);
						}
					}
				}
				throw new IllegalArgumentException("The search value was not found in the search range.");
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>search value</code>, <code>search range</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The relative position of the first ocuring <code>search value</code> in the <code>search range</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * <code>search range</code> should be a one dimensional range.<br>
	 * If the first cell in the <code>search range</code> is equal to the <code>search value</code>, than the result of MATCH is 1.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	MATCH("Returns the relative position of a search value in a range.", "<b>search value, search range</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			assertArgumentRange(1, arguments);
			
			String searchVal = stringValueOf(arguments[0]);
			Range matrix = ((Range)arguments[1]);
			int width = matrix.getColumnCount();
			int height = matrix.getRowCount();
			
			if(width != 1 && height != 1) {
				throw new IllegalArgumentException("The search range can have only one dimension, meaning a (part of a) column or a (part of a) row.");
			} else {
				int index = 0;
				for (Cell cell : matrix.getCellArray()) {
					index++;
					if(stringValueOf(cell.getValue()).equals(searchVal)) {
						return index;
					}
				}
				throw new IllegalArgumentException("The search value was not found in the search range.");
			}
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>cell reference</code>, <code>random cell</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Reference to the range specified by the string <code>cell reference</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The random cell is a cell in the sheet you want the INDIRECT reference to refer to.
	 * This is needed because without this cell, the function does not what sheet it should refer to.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	INDIRECT("Returns a reference to a cell you specify as a string, e.a.: \"B4\"", "<b>cell reference (string), random cell in sheet</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(2, arguments.length);
			Sheet sheet = ((Range) arguments[1]).firstCell().getSheet();
			String cellref = stringValueOf(arguments[0]);
			Parser parser = new Parser(sheet, cellref);
			Object result = parser.parse();
			return result;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>value</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The length of the <code>value</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	LENGTH("Returns the length of the argument.", "<b>value</b>") {
		@Override
		public Object calculate(Object... arguments) {
			assertArguments(1, arguments.length);
			return stringValueOf(arguments[0]).length();
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> none
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Surprise!</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The ANDY() is the most advanced feature in the Sandwitch spreadsheet application
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ANDY("Returns a surprise!", "<i>none on anything you like!</i>") {
		@Override
		public Object calculate(Object... arguments) {
			return "Hoi Andy!";
		}
	};
	
	/**
	 * Description of a function to be used in the function dialog.
	 */
	private final String description;
	
	/**
	 * List of expected arguments of a function.
	 */
	private final String argumentList;
	
	/**
	 * @return description
	 */
	public String getDescription() {
		return description;
	}
	
	/**
	 * @return argumentList
	 */
	public String getArgumentList() {
		return argumentList;
	}
	
	private final static String DEFAULT_DESCRIPTION = "No description available for this function";
	private final static String DEFAULT_ARGLIST= "Sorry, no argument list available.";
	
	/**
	 * Function constructor for a Function without a description and argument list.
	 */
	private Function() {
		this(DEFAULT_DESCRIPTION, DEFAULT_ARGLIST);
	}
	
	/**
	 * Function constructor for a Function with a description and without argument list.
	 * @param description
	 */
	private Function(String description) {
		this(description, DEFAULT_ARGLIST);
	}
	
	/**
	 * Function constructor for a Function with a description and a argument list.
	 * @param description
	 * @param argumentList
	 */
	private Function(String description, String argumentList) {
		this.description = description;
		this.argumentList = argumentList;
	}

	/**
	 * Calculate the value for a function. This method takes at least one object
	 * as parameter, but additional arguments are allowed. Parameters should be
	 * of the type <code>Integer</code>, <code>Double</code>,
	 * <code>Boolean</code>, <code>String</code>, <code>Reference</code>, or
	 * another <code>Function</code>.
	 * 
	 * @param arguments
	 *            Optional additional arguments
	 * @return <code>Object</code> of implemented type
	 */
	public abstract Object calculate(Object... arguments);
	
	/**
	 * Calculate the negative value for a function. This method takes at least
	 * one object as parameter, but additional arguments are allowed. Parameters
	 * should be of the type <code>Integer</code>, <code>Double</code>,
	 * <code>Boolean</code>, <code>String</code>, <code>Reference</code>, or
	 * another <code>Function</code>.
	 * 
	 * @param arguments
	 *            Optional additional arguments
	 * @return <code>Object</code> of implemented type
	 */
	public Object calculateNegative(Object... arguments) {
		Object result = calculate(arguments);
		if (result instanceof Integer) {
			return new Integer(-(Integer) result);
		} else if (result instanceof Double) {
			return new Double(-(Double) result);
		} else {
			return new Integer(-intValueOf(result));
		}
	}
	
	/**
	 * Get the <code>integer</code> value of an object. Values of the type
	 * <code>Double</code> will be rounded to the closest <code>integer</code>
	 * and <code>Booleans</code> will be converted to their <code>integer</code>
	 * value: <code>1</code> for <code>true</code> and <code>0</code> for
	 * <code>false</code>.
	 * 
	 * An empty String will return 0. A non-empty String will be casted to
	 * an integer, or throw a NumberFormatException.
	 * @param obj Object to convert
	 * @return <code>integer</code> value to calculate with
	 * @throws NumberFormatException
	 */
	public static int intValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).intValue();
		} else if ( obj instanceof String ) {
			return "".equals(obj) ? 0 : Integer.parseInt((String) obj);
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		} else if (obj instanceof Range) {
			return intValueOf(((Range) obj).getCellArray()[0]);
		} else if (obj instanceof Cell) {
			return intValueOf(((Cell) obj).getValue());
		}
		throw new IllegalArgumentException("Convert to integer error!");
	}

	/**
	 * Get the <code>double</code> value of an object. Values of the type
	 * <code>Boolean</code> will be converted to their <code>integer</code>
	 * value: <code>1</code> for <code>true</code> and <code>0</code> for
	 * <code>false</code>.
	 * 
	 * An empty String will return 0. A non-empty String will be casted to
	 * a double, or throw a NumberFormatException.
	 * 
	 * @param obj
	 *            Object to convert
	 * @return <code>double</code> value to calculate with
	 * @throws NumberFormatException
	 */
	public static double doubleValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).doubleValue();
		} else if ( obj instanceof String ) {
			return "".equals(obj) ? 0 : Double.parseDouble((String) obj);
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		} else if (obj instanceof Range) {
			return doubleValueOf(((Range) obj).getCellArray()[0]);
		} else if (obj instanceof Cell) {
			return doubleValueOf(((Cell) obj).getValue());
		}
		throw new IllegalArgumentException("Convert to double error!");
	}

	/**
	 * Logic operations will use the <code>boolean</code> type for calculations.
	 * Arguments with other types will be converted to a boolean value.
	 * <ul>
	 * <li><code>Integer</code> and <code>Double</code>-values with a value...
	 * <ul>
	 * <li>greater or equal to <code>1</code> will be parsed as <code>true</code></li>
	 * <li>less than <code>1</code> will be parsed as <code>false</code></li>
	 * </ul>
	 * <li>Values of the type <code>String</code> with a length...
	 * <ul>
	 * <li>greater than <code>1</code> will be parsed as <code>true</code></li>
	 * <li>else (empty String) will be parsed as <code>false</code></li>
	 * </ul>
	 * 
	 * @param obj
	 * 				Object to convert
	 * @return <code>boolean</code> value to calculate with
	 */
	public static boolean booleanValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).intValue() >= 1;
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE);
		} else if (obj instanceof String) {
			return ((String) obj).length() >= 1;
		} else if ( obj instanceof Range ) {
			return booleanValueOf(((Range) obj).getCellArray()[0]);
		} else if ( obj instanceof Cell ) {
			return booleanValueOf(((Cell) obj).getValue());
		}
		return false;
	}
	
	/**
	 * 
	 * @param obj Object to convert
	 * @return <code>boolean</code> value to calculate with
	 */
	public static String stringValueOf(Object obj) {
		if (obj instanceof Range) {
			return stringValueOf(((Range) obj).firstCell());
		} else if (obj instanceof Cell) {
			return ((Cell) obj).getValue().toString();
		} else if (obj != null) {
			return obj.toString();
		} else {
			throw new IllegalArgumentException("Covert to string error!");
		}
	}
	
	/**
	 * Get a function value by name. The name is converted to uppercase automatically
	 * 
	 * @param value
	 * @return Function matching the name
	 * @throws IllegalArgumentException
	 *             if the specified enum type has no constant with the specified
	 *             name, or the specified class object does not represent an
	 *             enum type
	 * @throws NullPointerException
	 *             if enumType or name is null
	 */
	public static Function get(String value) {
		return Function.valueOf(value.toUpperCase());
	}
	
	/**
	 * Method to convert unnecessary doubles to integers
	 * @param d is a double value
	 * @return the same value as the input, but is converted to Integer if there is no significant decimal part behind the comma
	 */
	public static Object convertToIntIfApplicable(double d) {
		if (Math.floor(d) == d) {
			return (int) d;
		}
		return d;
	}
	
	/**
	 * Checks if the number of arguments given to a function is equal to the number of arguments it requires.
	 * @param count is the number of arguments the function should get
	 * @param length is the number of arguments the function actualy got (arguments.length)
	 */
	public static void assertArguments(int count, int length) {
		if (count != length) {
			throw new IllegalArgumentException("This function requires " + count + " arguments, but " + length + " where supplied!");
		}
	}
	
	/**
	 * Checks if the number of arguments given to a function is equal to either one of two valid numbers of required arguments.
	 * @param count1 is the first number of arguments the function can handle
	 * @param count2 is the second number of arguments the function can handle
	 * @param length is the number of arguments the function actualy got (arguments.length)
	 */
	public static void assertTwoArguments(int count1, int count2, int length) {
		if (count1 != length && count2 != length) {
			throw new IllegalArgumentException("This function requires " + count1 + " or " + count2 + " arguments, but " + length + " where supplied!");
		}
	}
	
	/**
	 * Checks if the number of arguments given to a function is at least the minimum number of arguments the function requires.
	 * @param min is the minimum number of argumens the function should get
	 * @param length is the number of arguments the function actualy got (argument.length)
	 */
	public static void assertMinArguments(int min, int length) {
		if (min > length) {
			throw new IllegalArgumentException("This function requires at least " + min + " arguments, but " + length + " where supplied!");
		}
	}
	
	/**
	 * Checks if the number of arguments given to a function is not more than the function can handle.
	 * @param max is the maximum number of argumens the function should get
	 * @param length is the number of arguments the function actualy got (argument.length)
	 */
	public static void assertMaxArguments(int max, int length) {
		if (length > max) {
			throw new IllegalArgumentException("This function cannot handle more than " + max + " arguments, but " + length + " where supplied!");
		}
	}
	
	/**
	 * Checks if a certain argument in the array of arguments given to a function is instance of Range.
	 * @param index is the index of the argument in the array of arguments that must be a Range
	 * @param args is the array of arguments supplied to the function
	 */
	public static void assertArgumentRange(int index, Object... args) {
		if (!(args[index] instanceof Range)) {
			throw new IllegalArgumentException("This function requires argument " + (index + 1) + " to be a cell reference, but it is not!");
		}
	}
	
	/**
	 * Checks if a certain argument given to a function is instance of Range.
	 * @param arg is the argument that must be a Range
	 */
	public static void assertArgumentRange(Object arg) {
		if (!(arg instanceof Range)) {
			throw new IllegalArgumentException("This function requires a certain argument to be a cell reference, but it is not!");
		}
	}
	
	/**
	 * Checks if a certain argument given to a function is not a range with more than one cell in it.
	 * @throws an IllegalArgumentException only if the argument is instance of Range and contains more than 1 cell.
	 * @param arg is the argument that must be a Range
	 */
	public static void assertArgumentSingleRange(Object arg) {
		if ((arg instanceof Range) && ((Range)arg).getCellArray().length > 1) {
			throw new IllegalArgumentException("This function cannot handle ranges bigger than one cell.");
		}
	}
}
