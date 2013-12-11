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
	ADD(new char[] { '+' }, 5) {
		
		@Override
		Object calculate(Object first, Object second) {
			if ( first instanceof Integer ) {
				return new Integer((Integer) first + Function.intValueOf(second));
			} else if ( first instanceof Double ) {
				return new Double((Double) first + Function.doubleValueOf(second));
			} else if ( first instanceof Boolean ) {
				return ((Boolean) first).equals(Boolean.TRUE) || Function.booleanValueOf(second);
			} else if ( first instanceof String ) {
				return first.toString() + second.toString();
			}
			throw new IllegalArgumentException("#VALUE");
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
	SUBTRACT(new char[] { '-' }, 5) {
		
		@Override
		Object calculate(Object first, Object second) {
			if ( first instanceof Integer ) {
				return new Integer((Integer) first - Function.intValueOf(second));
			} else if ( first instanceof Double ) {
				return new Double((Double) first - Function.doubleValueOf(second));
			} else if ( first instanceof Boolean ) {
				return ((Boolean) first).equals(Boolean.TRUE) && !Function.booleanValueOf(second);
			} else if ( first instanceof String ) {
				return ((String) first).replace(second.toString(), "");
			}
			throw new IllegalArgumentException("#VALUE");
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
	MULTIPLY(new char[] { '*' }, 7) {
		
		@Override
		Object calculate(Object first, Object second) {
			if ( first instanceof Integer ) {
				return new Integer((Integer) first * Function.intValueOf(second));
			} else if ( first instanceof Double ) {
				return new Double((Double) first * Function.doubleValueOf(second));
			} else if ( first instanceof Boolean ) {
				return ((Boolean) first).equals(Boolean.TRUE) && Function.booleanValueOf(second);
			}
			throw new IllegalArgumentException("#VALUE");
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
	DIVIDE(new char[] { '/' }, 7) {
		
		@Override
		Object calculate(Object first, Object second) {
			if ( first instanceof Integer ) {
				return new Integer((Integer) first / Function.intValueOf(second));
			} else if ( first instanceof Double ) {
				return new Double((Double) first / Function.doubleValueOf(second));
			}
			throw new IllegalArgumentException("#VALUE");
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
	MODULUS(new char[] { '%' }, 7) {
		
		@Override
		Object calculate(Object first, Object second) {
			if ( first instanceof Integer ) {
				return new Integer((Integer) first % Function.intValueOf(second));
			} else if ( first instanceof Double ) {
				return new Integer(Function.intValueOf(first) % Function.intValueOf(second));
			}
			throw new IllegalArgumentException("#VALUE");
		}
	};

	char[] op;
	int precedence;

	/**
	 * Constructor for an operand. Binds an unique char array.
	 * 
	 * @param op
	 */
	Operator(char[] op, int precedence) {
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
		throw new IllegalArgumentException(new String(op).toString() + " is not a valid operator!");
	}
}