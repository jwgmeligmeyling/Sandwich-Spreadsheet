package Functions;

/**
 * ADD function
 * Returns FunctionA ^ 
 * @author Maarten Flikkema
 *
 */
public class Power extends Function {
	
	Function valueA;
	Function valueB;

	@Override
	public int valueOf() {
		return (int)Math.pow(valueA.valueOf(), valueB.valueOf());
	}
	
	@Override
	public String toString() {
		return null;
	}
}
