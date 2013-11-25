package Functions;

/**
 * Round function
 * Get the rounded value of a double
 * @author Maarten Flikkema
 */
public class Round extends Function {

	@Override
	public String toString() {
		return "ROUND(" + arguments[0].toString();
	}
	
	@Override
	public int toInteger() {
		return (int)Math.round(arguments[0].toDouble());
	}
	
	@Override
	public double toDouble() {
		return (double)Math.round(arguments[0].toDouble());
	}
}
