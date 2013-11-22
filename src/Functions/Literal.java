package Functions;

/**
 * Literal function is a function container for literals (eg. 5)
 * TODO References to cells.
 * @author jgmeligmeyling
 *
 */
public class Literal extends Function {
	
	int value;
	
	Literal() {}
	
	Literal(String value) {
		this.value = Integer.parseInt(value);
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
