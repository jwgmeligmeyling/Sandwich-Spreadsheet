package Functions;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * This class can be left alone.
 * Register new functions in the Functions ENUM (Functions.java)
 */
/**
 * Abstract class for parsing functions
 * @author jgmeligmeyling
 *
 */
public abstract class Function {
	
	private final static char OPENING_BRACKET = '(';
	private final static char CLOSING_BRACKET = ')';
	private final static char SEPARATOR = ',';
	private final static Pattern TAGS = Pattern.compile("[(),]");
	private final static Pattern NUMBER = Pattern.compile("^\\d+$");
	
	Function[] arguments;
	
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
	 * Method to parse Math strings
	 * @param input
	 * @return
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 */
	static Function parse(String input) throws InstantiationException, IllegalAccessException
	{
		Matcher m1 = NUMBER.matcher(input);
		
		if ( m1.find() )
		{
			return new Literal(input);
		}
		
		Matcher m2 = TAGS.matcher(input);
		int start = 0, end = 0, match, depth = 0, i = 0;
		char type;
		int[] separators = new int[input.length()/3];
		String name = null;		
		
		while(m2.find())
		{
			
			match = m2.start();
			type = input.charAt(match);
			
			switch (type)
			{
			
			case OPENING_BRACKET:
				if ( depth == 0 ) {
					start = match;
					name = input.substring(0, match);
				}
				depth++;
				break;
				
			case CLOSING_BRACKET:
				depth--;
				if ( depth == 0)
					end = match;
				break;
				
			case SEPARATOR:
				if ( depth == 1 ) 
					separators[i++] = match;
				break;
			}
			
		}
		
		separators[i++] = end;
		List<Function> args = new ArrayList<Function>();
			
		for ( int j = 0, from = start; j < i; j++ )
		{
			String s = input.substring(from + 1, separators[j]);
			Function f = Function.parse(s); 
			args.add(f);
			from = separators[j];
		}
			
		Function[]  arguments = new Function[args.size()];
		args.toArray(arguments);
		
		return Functions.valueOf(name).create(arguments);
	}
	
	/**
	 * Main method for debugging
	 * TODO remove this 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			Function f = Function.parse("ADD(ADD(ADD(1,2),ADD(3,2)),ADD(ADD(4,5),4))");
			System.out.println("Uitkomst van " + f.toString() + " = " + f.valueOf());
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
	}
}
