package Functions;

/**
 * ADD function
 * Returns FunctionA * FunctionB
 * @author jgmeligmeyling
 *
 */
public class Add extends Function
{
	
	@Override
	public int valueOf() {
		int value = 0;
		
		for ( Function arg : arguments)
		{
			value += arg.valueOf();
		}
		
		return value;
	}
	
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

}