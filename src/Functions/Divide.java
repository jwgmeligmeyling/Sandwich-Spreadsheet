package Functions;

/**
 * DIVIDE function
 * Function Divide returns: argument[0] / argument[1]
 * @author Maarten Flikkema
 */
public class Divide extends Function
{
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("POW(");
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
		return (int) Math.pow(arguments[0].toInteger(), arguments[1].toInteger());
	}

	@Override
	public double toDouble() {
		return Math.pow(arguments[0].toDouble(), arguments[1].toDouble());

	}
}
