package Functions;

/**
 * ADD function
 * Returns FunctionA * FunctionB
 * @author jgmeligmeyling
 *
 */
public class Add extends Function {
	
	Function valueA;
	Function valueB;

	@Override
	public int valueOf() {
		return valueA.valueOf() + valueB.valueOf();
	}
	
	@Override
	public String toString() {
		return null;
	}

}
