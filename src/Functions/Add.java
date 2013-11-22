package Functions;

/**
 * ADD function
 * Iterators over the Functions in the arguments array,
 * and returns te sum in the toInteger() and toDouble() methods.
 * @author Jan-Willem Gmelig Meyling
 *
 */
public class Add extends Function
{
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("ADD(");
		int i = 0, l = arguments.length;
		
		while (true)
		{
			sb.append(arguments[i].toString());
			
			if (++i < l ) {
				sb.append(",");
			} else {
				break;
			}
		}
		
		sb.append(")");
		return sb.toString();
	}

	@Override
	public int toInteger() {
		int value = 0;
		
		for ( Function arg : arguments)
		{
			value += arg.toInteger();
		}
		
		return value;
	}

	@Override
	public double toDouble() {
		double value = 0;
		
		for ( Function arg : arguments)
		{
			value += arg.toInteger();
		}
		
		return value;
	}

}
