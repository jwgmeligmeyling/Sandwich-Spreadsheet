package Functions;

/**
 * Literal function is a function container for literals (eg. 5)
 * TODO References to cells.
 * @author jgmeligmeyling
 *
 */
public class Literal extends Function {
	
	String value = "";
	
	Literal() {}
	
	Literal(String value) {
		this.value = value;
	}

	@Override
	public String toString() { 
		return value;
	}

	@Override
	public int toInteger() {
		return Integer.parseInt(value);
	}

	@Override
	public double toDouble() {
		return Double.parseDouble(value);
	}

}
