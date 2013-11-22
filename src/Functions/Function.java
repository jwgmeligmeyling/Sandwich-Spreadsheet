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
 * Abstract class for function. Functions should override some methods.
 * The Function class defines a static Parse method to convert Strings to Functions 
 * @author Jan-Willem Gmelig Meyling
 */
public abstract class Function {
	
	/*
	 * Constants
	 */
	private final static char OPENING_BRACKET = '(';
	private final static char CLOSING_BRACKET = ')';
	private final static char SEPARATOR = ',';
	private final static Pattern TAGS = Pattern.compile("[(),]");
	private final static Pattern NUMBER = Pattern.compile("^\\d+$");
	
	/*
	 * All instances of Function share this attribute
	 */
	Function[] arguments;
	
	/**
	 * @return {int} the integer value of the current function instance
	 */
	abstract public int toInteger();
	
	/**
	 * @return {double} the double value of the current function instance
	 */
	abstract public double toDouble();
	
	/**
	 * @return {String} The String representation of the current function. Used for editting.
	 */
	abstract public String toString();
	
	/**
	 * Method to parse Math strings
	 * @param {String} input
	 * @return {Function}
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 */
	static Function parse(String input) throws InstantiationException, IllegalAccessException
	{	
		/*
		 * If the input is a literal, return a new literal
		 */
		Matcher m1 = NUMBER.matcher(input);
		
		if ( m1.find() )
		{
			return new Literal(input);
		}
		
		/*
		 * Else, parse the function
		 */
		Matcher m2 = TAGS.matcher(input);
		int start = 0, end = 0, match, depth = 0, i = 0;
		char type;
		int[] separators = new int[input.length()/3];
		String name = null;		
		
		/*
		 * For every bracket or comma
		 */
		while(m2.find())
		{
			
			match = m2.start();
			type = input.charAt(match);
			
			switch (type)
			{
			
			case OPENING_BRACKET:
				/*
				 * If depth is zero, set start to the current match.
				 * Since the first match is now assumed as function, we fetch the function name at this point as well.
				 * Increment depth, because we're going one level of parentheses deeper.
				 */
				if ( depth == 0 ) {
					start = match;
					name = input.substring(0, match);
				}
				depth++;
				break;
				
			case CLOSING_BRACKET:
				/*
				 * Decrement depth, because we're going up one level
				 * If depth equals 0, we've arrived at the last closing bracket for the current function,
				 * set end to the current match.
				 */
				depth--;
				if ( depth == 0)
					end = match;
				break;
				
			case SEPARATOR:
				/*
				 * Ignore seperators when not at depth 1, else
				 * append the separator index to the separators array, 
				 * so we can split using these indexes later on.
				 */
				if ( depth == 1 ) 
					separators[i++] = match;
				break;
			}
			
		}
		
		/*
		 * Add the end to the separators array.
		 * Create an temporary ArrayList to store parsed arguments in.
		 */
		separators[i++] = end;
		List<Function> args = new ArrayList<Function>();
		
		/*
		 * Split the string into chuncks at the beginning bracket, separators and ending bracket.
		 */
		for ( int j = 0, from = start; j < i; j++ )
		{
			Function f = Function.parse(input.substring(from + 1, separators[j])); 
			args.add(f);
			from = separators[j];
		}
		
		/*
		 * Convert arguments ArrayList to an array, and call the matching function with the arguments.
		 */
		Function[]  arguments = new Function[args.size()];
		args.toArray(arguments);
		
		return Functions.valueOf(name).create(arguments);
	}

}
