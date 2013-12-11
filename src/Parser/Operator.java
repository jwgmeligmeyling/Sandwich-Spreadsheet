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
			return Function.ADD.calculate(first, second);
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
			return Function.MULTIPLY.calculate(first, second);
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

	GREATER_THAN(new char[] { '>' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) > Function
					.doubleValueOf(second));
		}

	},

	GREATER_OR_EQUAL(new char[] { '>', '=' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) >= Function
					.doubleValueOf(second));
		}

	},

	LESS(new char[] { '<' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) < Function
					.doubleValueOf(second));
		}

	},

	LESS_OR_EQUAL(new char[] { '<', '=' }, 8) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) <= Function
					.doubleValueOf(second));
		}

	},

	EQUAL(new char[] { '=', '=' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) == Function
					.doubleValueOf(second));
		}

	},

	DIFFERENT(new char[] { '<', '>' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) != Function
					.doubleValueOf(second));
		}

	},

	NOT_EQUAL(new char[] { '!', '=' }, 9) {

		@Override
		Object calculate(Object first, Object second) {
			return (Function.doubleValueOf(first) != Function
					.doubleValueOf(second));
		}

	},

	AND(new char[] { '&', '&' }, 14) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.booleanValueOf(first)
					&& Function.booleanValueOf(second);
		}

	},

	OR(new char[] { '|', '|' }, 14) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.booleanValueOf(first)
					|| Function.booleanValueOf(second);
		}

	},

	SHIFT_LEFT(new char[] { '<', '<' }, 7) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.intValueOf(first) << Function.intValueOf(second);
		}

	},

	SHIFT_RIGHT(new char[] { '>', '>' }, 7) {

		@Override
		Object calculate(Object first, Object second) {
			return Function.intValueOf(first) >> Function.intValueOf(second);
		}

	},
	
	CONCAT(new char[] {'&' }, 6) {
		@Override
		Object calculate(Object first, Object second) {
			return first.toString() + second.toString();
		}
	};

	char[] op;
	int precedence;
	Associativity associativity = Associativity.LEFT_TO_RIGHT;
	
	public enum Associativity {
		LEFT_TO_RIGHT,
		RIGHT_TO_LEFT;
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