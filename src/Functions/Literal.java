package Functions;

/**
 * Literal function is a function container for literals (eg. 5)
 * TODO References to cells.
 * @author jgmeligmeyling
 *
 */
public class Literal extends Function {
	int value = 0;
	
	/**
	 * Literal function is a function container for literals (eg. 5)
	 * TODO References to cells.
	 * @param input
	 */
	public Literal(String input) {
		value = Integer.parseInt(input);
	}

	@Override
	public int valueOf() {
		return value;
	}

	@Override
	public String toString() { 
		return Integer.toString(value);
	}

}
