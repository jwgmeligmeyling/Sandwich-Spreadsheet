package Functions;

/**
 * Subtract function
 * Get the subtraction of two Functions in the toInteger() and toDouble() methods.
 * @author Jan-Willem Gmelig Meyling
 *
 */
public class Subtract extends Function {

	@Override
	public String toString() {
		return "SUBTRACT(" + arguments[0].toString() + "," + arguments[1].toString() + ")";
	}

	@Override
	public int toInteger() {
		return arguments[0].toInteger() - arguments[1].toInteger();
	}

	@Override
	public double toDouble() {
		return arguments[0].toDouble() - arguments[1].toDouble();
	}

}
