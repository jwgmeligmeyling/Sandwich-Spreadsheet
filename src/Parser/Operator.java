package Parser;

import java.util.Arrays;

/**
 * Enum for the registered operators.
 * 
 * @author Jan-Willem Gmelig Meyling
 */
public enum Operator {
	/**
	 * Adds two operands
	 * 
	 * <pre>
	 * <b>Example</b>
	 * <em>10 + 20 will give 30</em>
	 * </pre>
	 */
	ADD(new char[] { '+' }, 6) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.SUM.calculate(first, second);
		}

	},

	/**
	 * Subtracts second operand from the first
	 * 
	 * <pre>
	 * <b>Example</b>
	 * <em>10 - 20 will give -10</em>
	 * </pre>
	 */
	SUBTRACT(new char[] { '-' }, 6) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.SUBTRACT.calculate(first, second);
		}

	},

	/**
	 * Multiply both operands
	 * 
	 * <pre>
	 * <b>Example</b>
	 * <em>10 * 20 will give 200</em>
	 * </pre>
	 */
	MULTIPLY(new char[] { '*' }, 5) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.PRODUCT.calculate(first, second);
		}

	},

	/**
	 * Divide numerator by denumerator
	 * 
	 * <pre>
	 * <b>Example</b>
	 * <em>20 / 10 will give 2</em>
	 * </pre>
	 */
	DIVIDE(new char[] { '/' }, 5) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.DIVIDE.calculate(first, second);
		}

	},

	/**
	 * Modulus Operator and remainder of after an integer division
	 * 
	 * <pre>
	 * <b>Example</b>
	 * <em>20 % 10 will give 0</em>
	 * </pre>
	 */
	MODULUS(new char[] { '%' }, 5) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.intValueOf(first) % Function.intValueOf(second);
		}
	},

	/**
	 * Return left operand to the power of the right operand
	 */
	POWER(new char[] { '^' }, 4) {

		@Override
		Object calculate(Object first, Object second) {
			double output = Function.doubleValueOf(first);
			output = Math.pow(output, Function.doubleValueOf(second));
			
			if ( Math.floor(output) == output ) {
				return new Integer((int) output);
			}

			return output;
		}
	},

	/**
	 * Return <code>true</code> if the value of operand A is greater than
	 * operand B
	 */
	GREATER_THAN(new char[] { '>' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) > Function
					.doubleValueOf(second));
		}

	},

	/**
	 * Return <code>true</code> if the value of operand A is greater than or
	 * equal to operand B
	 */
	GREATER_OR_EQUAL(new char[] { '>', '=' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) >= Function
					.doubleValueOf(second));
		}

	},

	/**
	 * Return <code>true</code> if the value of operand A is less than operand B
	 */
	LESS(new char[] { '<' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) < Function
					.doubleValueOf(second));
		}

	},

	/**
	 * Return <code>true</code> if the value of operand A is less than or
	 * equal to operand B
	 */
	LESS_OR_EQUAL(new char[] { '<', '=' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) <= Function
					.doubleValueOf(second));
		}

	},

	/**
	 * Return <code>true</code> if the value of operand A is equal to operand B
	 */
	EQUAL(new char[] { '=', '=' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) == Function
					.doubleValueOf(second));
		}

	},

	/**
	 *  Return <code>true</code> if the value of operand A is not equal to operand B
	 */
	DIFFERENT(new char[] { '<', '>' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return NOT_EQUAL.calculate(first, second);
		}

	},

	/**
	 *  Return <code>true</code> if the value of operand A is not equal to operand B
	 */
	NOT_EQUAL(new char[] { '!', '=' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) != Function
					.doubleValueOf(second));
		}

	},

	/**
	 *  Return <code>true</code> if the both operand A and operand B are <code>true</code>
	 */
	AND(new char[] { '&', '&' }, 14) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.booleanValueOf(first)
					&& Function.booleanValueOf(second);
		}

	},

	/**
	 *  Return <code>true</code> if either operand A or operand B is <code>true</code>.
	 */
	OR(new char[] { '|', '|' }, 14) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.booleanValueOf(first)
					|| Function.booleanValueOf(second);
		}

	},

	/**
	 *  Binary shift an integer value to the left with 0's.
	 */
	SHIFT_LEFT(new char[] { '<', '<' }, 7) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.intValueOf(first) << Function.intValueOf(second);
		}

	},

	/**
	 *  Binary shift an integer value to the right
	 */
	SHIFT_RIGHT(new char[] { '>', '>' }, 7) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.intValueOf(first) >> Function.intValueOf(second);
		}

	},
	
	/**
	 *  Concatenate two Strings
	 */
	CONCAT(new char[] { '&' }, 6) {
		@Override
		Object calculate(Object first, Object second) {
			return first.toString() + second.toString();
		}
	};

	/**
	 * The character sequence for this operator
	 */
	private final char[] op;
	/**
	 * The precedence for this operator
	 */
	public final int precedence;
	/**
	 * The associativity for this operator
	 */
	public final Associativity associativity = Associativity.LEFT_TO_RIGHT;
	
	/**
	 * An enumeration with types of associativity for operators
	 * @author Jan-Willem Gmelig Meyling
	 *
	 */
	public enum Associativity {
		LEFT_TO_RIGHT,
		RIGHT_TO_LEFT;
	}
	
	/**
	 * Method to determine if a operator has a higher precedence
	 * than another
	 * @param operator Another operator
	 * @return <code>true</code> if this operator has a higher precedence
	 */
	public boolean hasLowerOrEqualPrecedence(Operator operator) {
		return precedence <= operator.precedence;
	}

	/**
	 * Constructor for an operand. Binds an unique char array.
	 * 
	 * @param op
	 */
	private Operator(char[] op, int precedence) {
		this.op = op;
		this.precedence = precedence;
	}

	/**
	 * Calculate the value for a function. This method takes two arguments.
	 * Parameters should be of the type <code>Integer</code>,
	 * <code>Double</code>, <code>Boolean</code>, <code>String</code>,
	 * <code>Reference</code>, or another <code>Function</code>.
	 * 
	 * @param first
	 *            First argument
	 * @param second
	 *            Optional additional arguments
	 * @return <code>Object</code> of implemented type
	 */
	abstract Object calculate(Object first, Object second);

	public static Operator get(char[] op) {
		for (Operator o : Operator.values()) {
			if (Arrays.equals(op, o.op))
				return o;
		}
		throw new IllegalArgumentException(new String(op).toString()
				+ " is not a valid operator!");
	}
}