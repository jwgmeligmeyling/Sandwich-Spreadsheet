package Functions;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Abstract class for parsing functions
 * @author jgmeligmeyling
 *
 */
public abstract class Function {
	
	/**
	 * The value method returns the value of the current function instance
	 * @return
	 */
	abstract public int valueOf();
	
	/**
	 * The toString method returns the string representation of current function
	 */
	abstract public String toString();
	
	/**
	 * Static method to parse functions
	 * @param input
	 * @return
	 */
	static Function parse(String input) {
		Pattern p = Pattern.compile("^ADD\\((.*)\\,(.*)\\)$");
		Matcher m = p.matcher(input);

		if (m.find()) { // input matches ADD(#,#);
			Add add = new Add();
			add.valueA = Function.parse(m.group(1));
			add.valueB = Function.parse(m.group(2));
			return add;
		} else {
			return new Literal(input);
		}
		
	}
	
	/**
	 * Main method for debugging
	 * TODO remove this 
	 * @param args
	 */
	public static void main(String[] args) {
		Function f = Function.parse("ADD(ADD(5,4),4)");
		System.out.println("ValueOf F: " + f.valueOf() );
	}
}
