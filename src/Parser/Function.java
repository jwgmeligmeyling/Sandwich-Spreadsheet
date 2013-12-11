package Parser;

import File.Cell;
import File.Sheet.Range;

/**
 * Enumeration that binds functions to their function name.
 * 
 * Functions must implement a <code>calculate</code> method, which takes one or
 * more (arg, varargs) <code>Object</code> parameters of type
 * <code>Number</code>, <code>Boolean</code>, <code>Reference</code> or
 * <code>Range</code> and returns the result of the calculation as
 * <code>Number</code>.
 * 
 * @author Jan-Willem Gmelig Meyling
 */
public enum Function {

	/**
	 * The <code>ADD</code>-function adds the values of the arguments, and
	 * returns the result in the type of the first argument. For example: if the
	 * first argument is an instance of <code>Integer</code> and the second and
	 * third arguments are instances of <code>Double</code>, these arguments are
	 * rounded to the closest <code>Integer</code> and added to the result of
	 * type <code>Integer</code>. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 */
	ADD {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				int output = (Integer) first;
				for (Object argument : arguments) {
					output += intValueOf(argument);
				}
				return new Integer(output);
			} else if (first instanceof Double) {
				double output = (Double) first;
				for (Object argument : arguments) {
					output += doubleValueOf(argument);
				}
				return new Double(output);
			} else if (first instanceof Range) {
				double output = 0;
				for (Cell cell : ((Range) first).getCellArray()) {
					output += doubleValueOf(cell.getValue());
				}
				if ( Math.floor(output) == output ) {
					return new Integer((int) output);
				} else {
					return new Double(output);
				}
			}
			throw new IllegalArgumentException("#VALUE");
		}

	},

	/**
	 * The <code>SUBTRACT</code>-function subtracts the values of the arguments
	 * from the first argument, and returns the result in the type of the first
	 * argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second and third arguments are instances of
	 * <code>Double</code>, these arguments are rounded to the closest
	 * <code>Integer</code>. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 */
	SUBTRACT {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				int output = (Integer) first;
				for (Object argument : arguments) {
					output -= intValueOf(argument);
				}
				return new Integer(output);
			} else if (first instanceof Double) {
				double output = (Double) first;
				for (Object argument : arguments) {
					output -= doubleValueOf(argument);
				}
				return new Double(output);
			}
			throw new IllegalArgumentException("#VALUE");
		}

	},

	/**
	 * The <code>MULTIPLY</code>-function multiplies the values of the
	 * arguments, and returns the result in the type of the first argument. For
	 * example: if the first argument is an instance of <code>Integer</code> and
	 * the second and third arguments are instances of <code>Double</code>,
	 * these arguments are rounded to the closest <code>Integer</code>. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 */
	MULTIPLY {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				int output = (Integer) first;
				for (Object argument : arguments) {
					output *= intValueOf(argument);
				}
				return new Integer(output);
			} else if (first instanceof Double) {
				double output = (Double) first;
				for (Object argument : arguments) {
					output *= doubleValueOf(argument);
				}
				return new Double(output);
			}
			throw new IllegalArgumentException("#VALUE");
		}

	},

	/**
	 * The <code>DIVIDE</code>-function divides the values of the arguments from
	 * the first argument, and returns the result in the type of the first
	 * argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second and third arguments are instances of
	 * <code>Double</code>, these arguments are rounded to the closest
	 * <code>Integer</code>. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * 
	 * @author Maarten Flikkema
	 * @author Jan-Willem Gmelig Meyling
	 */
	DIVIDE {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				int output = (Integer) first;
				for (Object argument : arguments) {
					output /= intValueOf(argument);
				}
				return new Integer(output);
			} else if (first instanceof Double) {
				double output = (Double) first;
				for (Object argument : arguments) {
					output /= doubleValueOf(argument);
				}
				return new Double(output);
			}
			throw new IllegalArgumentException("#VALUE");
		}

	},

	/**
	 * The <code>POWER</code>-function takes two arguments, and returns the
	 * first argument to the power of the second argument, in the type of the
	 * first argument. For example: if the first argument is an instance of
	 * <code>Integer</code> and the second argument is an instances of
	 * <code>Double</code>, this argument rounded to the closest
	 * <code>Integer</code> for the calculation. <br>
	 * </br> When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * 
	 * @author Maarten Flikkema
	 * @author Jan-Willem Gmelig Meyling
	 */
	POWER {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				int a = (Integer) first;
				int b = intValueOf(arguments[0]);
				return new Integer((int) Math.pow(a, b));
			} else if (first instanceof Double) {
				double a = (Double) first;
				double b = doubleValueOf(arguments[0]);
				return new Double(Math.pow(a, b));
			}
			throw new IllegalArgumentException("#VALUE");
		}

	},

	/**
	 * Get the rounded value of a <code>Double</code>.
	 * 
	 * @author Maarten Flikkema
	 * @author Jan-Willem Gmelig Meyling
	 */
	ROUND {

		@Override
		Object calculate(Object first, Object... arguments) {
			if (first instanceof Integer) {
				return first;
			} else if (first instanceof Double) {
				return new Integer(intValueOf(first));
			}
			throw new IllegalArgumentException("#VALUE");
		}

	};

	/**
	 * Calculate the value for a function. This method takes at least one object
	 * as parameter, but additional arguments are allowed. Parameters should be
	 * of the type <code>Integer</code>, <code>Double</code>,
	 * <code>Boolean</code>, <code>String</code>, <code>Reference</code>, or
	 * another <code>Function</code>.
	 * 
	 * @param first
	 *            First argument
	 * @param arguments
	 *            Optional additional arguments
	 * @return <code>Object</code> of implemented type
	 */
	abstract Object calculate(Object first, Object... arguments);

	/**
	 * Calculate the negative value for a function. This method takes at least
	 * one object as parameter, but additional arguments are allowed. Parameters
	 * should be of the type <code>Integer</code>, <code>Double</code>,
	 * <code>Boolean</code>, <code>String</code>, <code>Reference</code>, or
	 * another <code>Function</code>.
	 * 
	 * @param first
	 *            First argument
	 * @param arguments
	 *            Optional additional arguments
	 * @return <code>Object</code> of implemented type
	 */
	Object calculateNegative(Object first, Object... arguments) {
		Object result = calculate(first, arguments);
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
	 * <code>false</code>. Values of the type <code>String</code> will always be
	 * converted to <code>0</code>.
	 * 
	 * @param obj
	 *            Object to convert
	 * @return <code>integer</code> value to calculate with
	 */
	public static int intValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).intValue();
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		}
		return 0;
	}

	/**
	 * Get the <code>double</code> value of an object. Values of the type
	 * <code>Boolean</code> will be converted to their <code>integer</code>
	 * value: <code>1</code> for <code>true</code> and <code>0</code> for
	 * <code>false</code>. Values of the type <code>String</code> will always be
	 * converted to <code>0</code>.
	 * 
	 * @param obj
	 *            Object to convert
	 * @return <code>double</code> value to calculate with
	 */
	public static double doubleValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).doubleValue();
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		}
		return 0;
	}

	/**
	 * Logic operations will use the <code>boolean</code> type for calculations.
	 * Arguments with other types will be converted to a boolean value.
	 * <code>Integer</code> and <code>Double</code>-values with a value greater
	 * or equal to <code>1</code> will be parsed as <code>true</code>, values
	 * less than <code>1</code> will be parsed as <code>false</code>. Values of
	 * the type <code>String</code> with a length greater than <code>1</code>
	 * will be parsed as <code>true</code>, else <code>false</code>.
	 * 
	 * @param obj
	 *            Object to convert
	 * @return <code>boolean</code> value to calculate with
	 */
	public static boolean booleanValueOf(Object obj) {
		if (obj instanceof Number) {
			return ((Number) obj).intValue() >= 1;
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE);
		} else if (obj instanceof String) {
			return ((String) obj).length() >= 1;
		}
		return false;
	}

	/**
	 * Get a function value by name. The name is converted to uppercase
	 * automatically
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

}
