package Parser;

import java.text.SimpleDateFormat;
import File.Cell;
import File.Sheet.Range;

/**
 * Enumeration that binds functions to their function name.<br>
 * Functions must implement a <code>calculate</code> method, which takes one or
 * more (arg, varargs) <code>Object</code> parameters of type
 * <code>Number</code>, <code>Boolean</code>, <code>Reference</code> or
 * <code>Range</code> and returns the result of the calculation as
 * <code>Number</code>.
 * 
 * @author Jan-Willem Gmelig Meyling <i>(functions, other)</i>
 * @author Maarten Flikkema <i>(functions)</i>
 */
public enum Function {
	
	/**
	 * <div> <b>Expected arguments:</b> <code>[argument]</code>,
	 * <code>[oneindig argument]...</code> </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * The <code>ADD</code>-function adds the values of the arguments, and
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
	 * When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
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
	 * <div>
	 * <b>Expected arguments:</b> <code>starting number</code>, <code>mult. number</code>, <code>[mult. numbers...]</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
	 * </ul>
	 * </div>
	 * The <code>MULTIPLY</code>-function multiplies the values of the
	 * arguments, and returns the result in the type of the first argument. For
	 * example: if the first argument is an instance of <code>Integer</code> and
	 * the second and third arguments are instances of <code>Double</code>,
	 * these arguments are rounded to the closest <code>Integer</code>.<br>
	 * When the input doesn't make any sense (adding a <code>String</code>
	 * or <code>Boolean</code>) a IllegalArgumentException is thrown.
	 * </div><br>
	 * <div><b>Authors:</b>
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
	 * <div>
	 * <b>Expected arguments:</b> <code>[argument]</code>, <code>[oneindig argument]...</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>[return omschrijving]</li>
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
	POWER {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 2;
			// @Jan-Willem: is het nodig om dit te asserten? - Maarten
			// Je wilt dit juist niet asserten, dan werken references niet meer
			
			double output = Math.pow(doubleValueOf(arguments[0]), doubleValueOf(arguments[1]));
			if (Math.floor(output) == output) {
				return new Integer((int) output);
			}
			return output;
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>number</code> As Double
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>Mathematicaly correct rounded value of <code>number</code></li>
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
	ROUND {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 1;
			return intValueOf(arguments[0]);
			// HO HO HO!!! Klopt dit wel??? Zie functie-omschrijving!!!!!!! - Maarten
			// Test het en we zullen het zien ;)
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <i>None</i>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>A random <code>Double</code> number</li>
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
	RAND {
		@Override
		Object calculate(Object... arguments) {
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
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * <li>[naam auteur]</li>
	 * </ul>
	 * </div>
	 */
	RANDBETWEEN {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 2;
			int a = intValueOf(arguments[0]);
			int b = intValueOf(arguments[1]);
			return (int) (Math.random() * ( b - a ) + a);
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
	 * Get the square root of a value. Returns an <code>integer</code> when
	 * applicable, else it returns a double.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
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
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The sin of <code>real number</code></li>
	 * </ul>
	 * </div>
	 * <div><b>Comments:</b><br>
	 * Get the sin of a value. Returns a <code>Double</code> value.
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
	 */
	SIN {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 0;
			return Math.sin(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>real number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>The cos of <code>real number</code></li>
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
	COS {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length == 0;
			return Math.cos(doubleValueOf(arguments[0]));
		}
	},
	
	/**
	 * <div>
	 * <b>Expected arguments:</b> <code>logical test</code>, [<code>value if true</code>], [<code>value if false</code>]
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
	IF {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length >= 1 && arguments.length <= 3;
			// Weet je zeker dat je zoveel wilt checken?
			// 4 argumenten geven nu een error, terwijl je ook gewoon het vierde argument zou kunnen negeren
			// Een argument van type Range wordt nu niet geaccepteerd, omdat het geen Boolean is.
			// booleanValueOf() zou dit mooi oplossen, dus eigenlijk kan je de
			// check gewoon weglaten.			
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
	 * <div><b>Comments:</b><br>
	 * ...
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten</li>
	 * </ul>
	 * </div>
	 */
	OR {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length >= 1;
			
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
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Maarten Flikkema</li>
	 * </ul>
	 * </div>
	 */
	AND {
		@Override
		Object calculate(Object... arguments) {
			assert arguments.length >= 1;
			
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
	 * <b>Expected arguments:</b> <code>number</code>
	 * </div><br>
	 * <div><b>Returns:</b>
	 * <ul>
	 * <li>If the argument is positive, the <code>SIGN</code> function will return <code>1</code></li>
	 * <li>If the argument is negative, the <code>SIGN</code> function will return <code>-1</code></li>
	 * <li>If the argument is zero, the <code>SIGN</code> function will return <code>0</code></li>
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
	SIGN {
		@Override
		Object calculate(Object... arguments) {
			// TODO de Excel variant werkt ook met ranges van 1 cell bij mij?
			assert arguments.length == 1;
			int temp = 0;
			if (doubleValueOf(arguments[0]) < 0) {
				temp = -1;
			} else if (doubleValueOf(arguments[0]) > 0) {
				temp = 1;
			}
			return temp;
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
	 * <div><b>Comments:</b><br>
	 * [opmerkingen]
	 * </div><br>
	 * <div><b>Authors:</b>
	 * <ul>
	 * <li>Jan-Willem Gmelig Meyling</li>
	 * </ul>
	 * </div>
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

}
