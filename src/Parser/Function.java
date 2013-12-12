package Parser;

import java.text.SimpleDateFormat;

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
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ADD {
		@Override
		Object calculate(Object... arguments) {
			double output = 0;
			
			for ( Object argument : arguments ) {
				if ( argument instanceof Range ) {
					for ( Cell cell : ((Range) argument).getCellArray() ) {
						output += doubleValueOf(cell.getValue());
					}
				} else {
					output += doubleValueOf(argument);
				}
			}
			
			if ( Math.floor(output) == output ) {
				return new Integer((int) output);
			}

			return output;
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
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SUBTRACT {

		@Override
		Object calculate(Object... arguments) {
			assert arguments.length > 1;
			double output = doubleValueOf(arguments[0]);
			
			for ( int i = 1; i < arguments.length; i++ ) {
				output -= doubleValueOf(arguments[i]);
			}
			
			if ( Math.floor(output) == output ) {
				return new Integer((int) output);
			}
			
			return output;
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
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	MULTIPLY {

		@Override
		Object calculate(Object... arguments) {
			assert arguments.length > 1;
			double output = doubleValueOf(arguments[0]);
			
			for ( int i = 1; i < arguments.length; i++ ) {
				output *= doubleValueOf(arguments[i]);
			}
			
			if ( Math.floor(output) == output ) {
				return new Integer((int) output);
			}

			return output;
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
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	DIVIDE {

		@Override
		Object calculate(Object... arguments) {
			assert arguments.length > 1;
			double output = doubleValueOf(arguments[0]);
			
			for ( int i = 1; i < arguments.length; i++ ) {
				output /= doubleValueOf(arguments[i]);
			}
			
			if ( Math.floor(output) == output ) {
				return new Integer((int) output);
			}

			return output;
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
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	POWER {

		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 2;
			double output = Math.pow(doubleValueOf(arguments[0]),
					doubleValueOf(arguments[1]));

			if (Math.floor(output) == output) {
				return new Integer((int) output);
			}
			
			return output;
		}

	},

	/**
	 * Get the rounded value of a <code>Double</code>.
	 * 
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	ROUND {

		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 1;
			return intValueOf(arguments[0]);
		}

	},
	
	/**
	 * Get a random <code>Double</code> value
	 */
	RAND {
		@Override
		Object calculate(Object... arguments) {
			return Math.random();
		}
	},
	
	/**
	 * Get the square root of a value. Returns an <code>integer</code> when
	 * applicable, else it returns a double.
	 */
	SQRT {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 0;
			double output = Math.sqrt(doubleValueOf(arguments[0]));
			
			if (Math.floor(output) == output) {
				return (int) output;
			}
			
			return output;
		}
	},
	
	/**
	 * Get the sin of a value. Returns a <code>Double</code> value.
	 */
	SIN {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 0;
			return Math.sin(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * Get the cos of a value. Returns a <code>Double</code> value.
	 */
	COS {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 0;
			return Math.cos(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * Create a date from a format string
	 */
	DATE {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 1;
			return (new SimpleDateFormat(arguments[0].toString())).format(System.currentTimeMillis());
		}
	};

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
	abstract Object calculate(Object... arguments);

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
	Object calculateNegative(Object... arguments) {
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
	 * <code>false</code>. Values of the type <code>String</code> will always be
	 * converted to <code>0</code>.
	 * 
	 * @param obj
	 *            Object to convert
	 * @return <code>integer</code> value to calculate with
	 */
	public static int intValueOf(Object obj) {
		/*
		 * TODO Numbers without '='
		 */
		if (obj instanceof Number) {
			return ((Number) obj).intValue();
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		} else if ( obj instanceof Range ) {
			return intValueOf(((Range) obj).getCellArray()[0]);
		} else if ( obj instanceof Cell ) {
			return intValueOf(((Cell) obj).getValue());
		}
		throw new IllegalArgumentException("#VALUE");
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
		/*
		 * TODO Numbers without '='
		 */
		if (obj instanceof Number) {
			return ((Number) obj).doubleValue();
		} else if (obj instanceof Boolean) {
			return ((Boolean) obj).equals(Boolean.TRUE) ? 1 : 0;
		} else if ( obj instanceof Range ) {
			return doubleValueOf(((Range) obj).getCellArray()[0]);
		} else if ( obj instanceof Cell ) {
			return doubleValueOf(((Cell) obj).getValue());
		}
		throw new IllegalArgumentException("#VALUE");
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
