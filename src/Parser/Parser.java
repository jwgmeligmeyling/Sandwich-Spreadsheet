package Parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;

/**
 * Class to parse the String
 * 
 * @author Jan-Willem Gmelig Meyling
 * 
 */
public class Parser {

	/**
	 * A final variable to store the <code>String</code> that is being parsed.
	 */
	private final String input;

	/**
	 * An final <code>int</code> to store the length of the input
	 * <code>String</code>.
	 */
	private final int length;

	/**
	 * A variable to store the current <code>Sheet</code>, so <code>Cell</code> and
	 * <code>Range</code> types can be accessed.
	 */
	private final Sheet sheet;
	
	/**
	 * A variable to store the current <code>Cell</code>
	 */
	private final Cell cell;

	/**
	 * An <code>int</code> to store the current index. Because we increment the
	 * value in the <code>next</code> method, we start at <code>-1</code> so the
	 * value always matches the current index.
	 */
	private int index = -1;
	
	/**
     * A value to store the current value of the peekIndex in. The peek index is
     * used to peek for a character without changing the current index.
     */
	private int peekIndex;

	/**
	 * A <code>char</code> to store the current character.
	 */
	private char current;

	/**
	 * A value to store the current depth in brackets. Used to determine which
	 * closing bracket closes to the first opening bracket. When we go one level
	 * of parentheses deeper, this value increments, allowing to ignore data
	 * between the parentheses and parse these recursively instead.
	 */
	private int depth = 0;

	/**
	 * A value to store the index of the open bracket we're working on. Used to
	 * split function arguments and to recursively call the new parser on a
	 * substring.
	 */
	private int openBracket = 0;

	/**
	 * A value to store the index of the bracket that closes the first open
	 * bracket. Used to split function arguments and to recursively call the new
	 * parser on a substring.
	 */
	private int closeBracket = 0;

	/**
	 * A value to store whether a <code>Number</code> or <code>Function</code>
	 * -output should be converted to it's negative value.
	 */
	private boolean isNegative = false;

	/**
	 * A variable to store the function currently being parsed. This value
	 * should be <code>null</code> when no function is being parsed, so
	 * expressions - which require different behaviour of the parentheses - are
	 * parsed correctly.
	 */
	private Function function;
	
	/**
	 */
	private Object value;
	
	/**
	 * This list is used to store the function arguments.
	 */
	List<Object> arguments = new ArrayList<Object>();

	/**
	 * This stack is used for pushing and popping values.
	 */
	Stack<Object> values = new Stack<Object>();

	/**
	 * This stack is used for pushing and popping operators.
	 */
	Stack<Operator> operators = new Stack<Operator>();

	/**
	 * Public Parse constructor. Takes a <code>String</code> as argument.
	 * @param sheet
	 * @param string
	 */
	public Parser(Sheet sheet, String string) {
		this.sheet = sheet;
		this.cell = null;
		length = string.length();
		input = string;
	}
	
	/**
	 * Public Parse constructor. Takes a <code>String</code> as argument.
	 * @param cell
	 */
	public Parser(Cell cell) {
		this.cell = cell;
		this.sheet = cell.getSheet();
		input = cell.getInput().replaceAll("\\s+", "");
		length = input.length();
		value = preparse();
	}

	/**
	 * Private Parse constructor. Used for recursive parsing between
	 * parentheses.
	 * 
	 * @param parser
	 *            Another parse instance
	 * @param from
	 *            From index
	 * @param to
	 *            To index
	 */
	private Parser(Parser parser, int from, int to) {
		sheet = parser.sheet;
		cell = parser.cell;
		input = parser.input;
		index = from;
		length = to;
	}

	/**
	 * Public parse method. Initiates the parsing of the current instance, and
	 * returns the result.
	 * 
	 * @return Result in the type of <code>Integer</code>, <code>Double</code>,
	 *         <code>Boolean</code> or <code>String</code>
	 * @throws IllegalArgumentException
	 *             If a undefined <code>Function</code> or <code>Operator</code>
	 *             was called.
	 * @throws NumberFormatException
	 *             If the string does not contain a parsable number.
	 */
	public Object parse() {
		if ( value != null ) {
			return value;
		}
		
		while (hasNext()) {
			switch (current = next()) {
			case ' ':
			case ':':
				continue;
			case '"':
			case '\'':
				getString();
				break;
			case '(':
				openBracket();
				break;
			case ')':
				closeBracket();
				break;
			case ',':
			case ';':
				argumentSeparator();
				break;
			case '<':
			case '>':
			case '^':
			case '|':
			case '&':
			case '=':
			case '!':
			case '~':
			case '+':
			case '-':
			case '*':
			case '/':
			case '%':
				getOperator();
				break;
			default:
				if (depth > 0) {
					break;
				}
				
				Boolean b = getBoolean();
				if ( b != null ) {
					values.push(b);
					break;
				}
				
				Object reference = getReference();
				if ( reference != null ) {
					if ( cell != null ) {
						if ( reference instanceof Range ) {
							cell.listen( (Range) reference);
							values.push(reference);
						} else if ( reference instanceof Cell ) {
							cell.listen( (Cell) reference);
							values.push(((Cell) reference).getValue());
						}
					}
					break;
				}

				if (Character.isDigit(current)) {
					Number n = getNumber();
					values.push(n);
					break;
				}

				getFunction();
				break;
			}
		}

		/*
		 * Calculate remaining values in the stack, and return the final result
		 */
		while (values.size() > 1) {
			calculate();
		}

		return values.pop();
		
	}

	/**
	 * Preparse the string, slices off the '=' sign, and looks for literals (5,
	 * 2.1, true/false) or returns the string
	 */
	private Object preparse() {
		if ( hasNext() ) {
			current = next();
			if  ( current == '=' ) {
				return null;
			} else {
				Boolean b = getBoolean();
				if ( b != null && !hasNext()) {
					return b;
				} else if (Character.isDigit(current)) {
					Number n = getNumber();
					if (!hasNext()) {
						return n;
					}
				}
				return input;
			}
		} else {
			return input;
		}
	}

	/**
	 * When a bracket is opened, the <code>depth</code> of this
	 * <code>Parser</code> instance is incremented. If the <code>
	 * depth</code> equals <code>1</code>, then set the <code>openBracket</code>
	 * value to the current <code>index</code>, so that we keep this value for
	 * later use.
	 */
	private void openBracket() {
		depth++;
		if (depth == 1) {
			openBracket = index;
		}
	}

	/**
	 * When a bracket is closed, the <code>depth</code> of this
	 * <code>Parser</code> instance is decremented. If the <code>depth</code>
	 * now equals <code>0</code> - and thus all opened brackets are closed - we
	 * push the calculated <code>value</code> to the <code>value stack</code>.
	 * If the <code>value</code> is a <code>Function</code> result, fetch the
	 * arguments first and calculate the <code>value</code>.
	 * 
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul><li>Jan-Willem Gmelig Meyling</li></ul>
	 * </div>
	 */
	private void closeBracket() {
		depth--;
		if (depth == 0) {
			closeBracket = index;

			if (function != null) {
				/*
				 * Since we're parsing a function, we need to push the last
				 * argument to the argument list, by calling the
				 * argumentSeparator function.
				 */
				argumentSeparator();
				/*
				 * Splice the first argument of the argument list to an Object
				 * and convert the tail of the argument list to an array of
				 * Objects, so that the function can be called with varargs.
				 */
				Object[] args = new Object[arguments.size()];
				arguments.toArray(args);
				/*
				 * When the value needs to be negative, calculate the negative
				 * value, else, calculate the normal value.
				 */
				Object value = (isNegative) ? function.calculateNegative(args)
						: function.calculate(args);
				/*
				 * Push the value to the value stack, and clear the arguments
				 * list and function variable, so that other expressions are
				 * parsed correctly.
				 */
				values.push(value);
				arguments.clear();
				function = null;
			} else {
				/*
				 * Parse the expression between the parentheses recursively, by
				 * constructing a new Parser instance.
				 */
				values.push(new Parser(this, openBracket, closeBracket).parse());
			}
		}
	}
	
	/**
	 * To calculate a value, pop two value objects from the value stack, and one
	 * <code>Operator</code> from the operator stack. Then calculate the value,
	 * for the operation of the operator with the two arguments, and push the
	 * result to the value stack.
	 * 
	 * @throws EmptyStackException
	 *             if this value stack or operator stack is empty.
	 * 
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul><li>Jan-Willem Gmelig Meyling</li></ul>
	 * </div>
	 */
	private void calculate() {
		Object a = values.pop();
		Object b = values.pop();
		values.push(operators.pop().calculate(b, a));
	}

	/**
	 * This method pushes an <code>Operator</code> to the operator stack. When
	 * the operator stack isn't empty, we peek for the last
	 * <code>Operator</code>. If the <code>precedence</code> of the
	 * <code>Operator</code> in the stack is higher than or equal to the
	 * <code>precedence</code> of the <code>Operator</code> supplied as
	 * argument, we calculate this value first.
	 * 
	 * <pre>
	 * This is such that 5 * 5 + 5 converts to 25 + 5.
	 * </pre>
	 * 
	 * See also Dijkstra's Two Stack algorithm.
	 * 
	 * @param operator
	 *            The operator to push
	 * 
	 * <div>
	 * <b>Authors:</b><br>
	 * <ul><li>Jan-Willem Gmelig Meyling</li></ul>
	 * </div>
	 */
	private void pushOperator(Operator operator) {
		while (!operators.empty()
				&& operators.peek().hasLowerOrEqualPrecedence(operator)) {
			calculate();
		}
		operators.push(operator);
	}

	/**
	 * Create a new argument separator
	 */
	private void argumentSeparator() {
		if (depth < 2 && function != null) {
			if ( openBracket < index - 1 ) {
				Object value = new Parser(this, openBracket, index).parse();
				arguments.add(value);
			}
			openBracket = index;
		}
	}

	/**
	 * Method to parse a <code>Boolean</code> from the input string.
	 * 
	 * @return a <code>Boolean</code> object holding the value represented by the
	 *         <code>String</code>-argument.
	 */
	private Boolean getBoolean() {
		if (searchWord("TRUE")) {
			return true;
		} else if (searchWord("FALSE")) {
			return false;
		}
		return null;
	}
	
	/**
	 * Method to parse a <code>Number</code> from the input string.
	 * 
	 * @return a <code>Number</code> object holding the value represented by the
	 *         <code>String</code>-argument.
	 * @throws NumberFormatException
	 *             if the string does not contain a parsable number.
	 */
	private Number getNumber() throws NumberFormatException {
		String s = (isNegative) ? "-" : "";
		isNegative = false;
		char c = current;
		boolean d = false;
		peekIndex = index + 1;
		do {
			s += c;
			c = peek();
			if (!Character.isDigit(c)) {
				if (c == '.' || c == 'E' || c == 'e') {
					d = true;
				} else {
					break;
				}
			}
			peekIndex++;
		} while (true);
		
		index = peekIndex -1;
		if (d) {
			return Double.valueOf(s);
		} else {
			return Integer.valueOf(s);
		}
	}

	/**
	 * Method to get a <code>String</code> from the input. Expects the current
	 * character to be the start of a string - a quote basically. Then it skips
	 * to the next quote and pushes the substring to the value <code>Stack</code>.
	 */
	private void getString() {
		if (depth == 0) {
			int start = index + 1;
			skipToNext(current);
			values.push(input.substring(start, index));
		}
	}
	
	/**
	 * These characters are the allowed characters for the operators that take
	 * two characters (for example the <code>>=</code> operator)
	 */
	private final static String SECOND_OPERATOR_CHARACTERS = "=<>&|";

	/**
	 * Method to peek for an operator at the current position in the
	 * <code>String</code>.
	 * 
	 * @throws IllegalArgumentException
	 *             if no such <code>Operator</code> is implemented
	 */
	private void getOperator() {
		if (current == '-' && depth == 0 
				&& (index == 0 || "+-*/<>~!&|^=(,".indexOf(previous()) != -1)) {
			/*
			 * Parse -5 and x-5 differently :)
			 */
			isNegative = true;
		} else if (depth == 0) {
			peekIndex = index + 1;
			char[] op;
			/*
			 * Look if the operator uses a second character (">=")
			 */
			char c = peek();
			if (SECOND_OPERATOR_CHARACTERS.indexOf(c) != -1) {
				op = new char[] { current, c };
				peekIndex++;
				index = peekIndex -1;
			} else {
				op = new char[] { current };
			}
			/*
			 * Try to get the Operator instance that matches this character
			 * array and push it to the operator stack.
			 */
			Operator o = Operator.get(op);
			pushOperator(o);
		}
	}

	/**
	 * Method to peek for a function at the current position in the
	 * <code>String</code>.
	 * 
	 * @throws IllegalArgumentException
	 *             if the specified enum type has no constant with the specified
	 *             name, or the specified class object does not represent an
	 *             enum type
	 */
	private void getFunction() {
		String s = "";
		char c = current;
		peekIndex = index;
		/*
		 * Characters need to be in the range [A-z]. Append these characters to
		 * the function name String. If a non-alphabetic character is found, we
		 * break out of the while loop.
		 */
		while (Character.isLetter(c)) {
			s += c;
			peekIndex++;
			c = peek();
		}
		/*
		 * If the String isn't empty, we try to fetch the function. This
		 * function will throw an IllegalArgumentException if no Operator is
		 * found. We catch this Exception, and then return null.
		 */
		if (!s.isEmpty()) {
			function = Function.get(s);
			index = peekIndex -1;
		}
	}
	
	/**
	 * Fetch a reference. A reference can be in several formats:
	 * <ul>
	 * <li>A reference to a single cell, matches one or more letters followed by
	 * one or more digits ("A3")</li>
	 * <li>A reference to one or more columns, matches one or more letters,
	 * followed by a colon and the same pattern ("A:B")</li>
	 * <li>A reference to a row, matches one or more digits, followed by a colon
	 * and the same character pattern ("1:3")</li>
	 * <li>A reference to a range of cells, matches two single cell
	 * representations described above, separated by a colon. ("A1:B3")</li>
	 * </ul>
	 * 
	 * @return <code>Object</code> of type <code>Cell</code>,
	 *         <code>Column</code>, <code>Row</code> or <code>Range</code>, or
	 *         <code>null</code> if the character sequence did not match a
	 *         reference.
	 * 
	 * @throws IllegalArgumentException
	 *             When the input of the reference is malformed.
	 */
	private Object getReference() {
		/*
		 * Current implementation only supports Cells (A1) or Ranges (A1:B2)
		 * TODO Support for rows and columns (?)
		 */
		peekIndex = index;
		Cell a = getCell();
		if (a != null) {
			if (peek() == ':') {
				peekIndex++;
				Cell b = getCell();
				if (b != null) {
					index = peekIndex -1;
					Range range = sheet.getRange(a,b);
					if ( cell != null && range.contains(cell) )
						throw new IllegalArgumentException("Cross reference!");
					return sheet.getRange(a, b);
				} else {
					throw new IllegalArgumentException(
							"Expected a column reference after :");
				}
			} else if (cell != null && a.equals(cell) ) {
				throw new IllegalArgumentException("Cross reference!");
			} else {
				index = peekIndex -1;
				return a;
			}
		}
		return null;
	}

	/**
	 * Method to peek for a <code>Cell</code> at current index. A cell is
	 * expected to be in the format "A1"
	 * 
	 * @return <code>Cell</code> in the current <code>Sheet</code>, or
	 *         <code>null</code> if no <code>Cell</code> was found either in the
	 *         input String or the <code>Sheet</code>.
	 */
	private Cell getCell() {
		int colIndex = getColIndex();
		if (colIndex != -1) {
			int rowIndex = getRowIndex();
			if (rowIndex != -1) {
				return sheet.getCellAt(colIndex, rowIndex);
			}
		}
		return null;
	}

	/**
	 * Method to peek for a String representation of an column index. Column
	 * indexes are expected to be in the format A, B, C... When a column is
	 * found, the current index is incremented.
	 * 
	 * @return The index of the column, starting at 0 for A, or -1 when no
	 *         column index was found.
	 */
	private int getColIndex() {
		int colIndex = 0;
		for (;;) {
			char c = Character.toUpperCase(peek());
			if (Character.isLetter(c)) {
				colIndex = colIndex * 26 + c - 64;
				peekIndex++;
			} else if (colIndex <= 0) {
				return -1;
			} else {
				break;
			}
		}
		return colIndex - 1;
	}

	/**
	 * Method to peek for an row index. Row indexes are expected to be in the
	 * format 1, 2, 3...
	 * 
	 * @return The index of the row, starting at 0 for 1, or -1 when no row
	 *         index was found.
	 */
	private int getRowIndex() {
		int rowIndex = 0;
		for (;;) {
			char c = peek();
			if (Character.isDigit(c)) {
				rowIndex = rowIndex * 10 + Character.getNumericValue(c);
				peekIndex++;
			} else if (rowIndex <= 0) {
				return -1;
			} else {
				break;
			}
		}
		return rowIndex - 1;
	}

	/**
	 * Method to determine if there is a next character
	 * 
	 * @return true if there is a next character
	 */
	private boolean hasNext() {
		return (index + 1) < length;
	}

	/**
	 * Return the next character in the <code>String</code>, and increment the
	 * current <code>index</code>.
	 * 
	 * @return next character
	 */
	private char next() {
		return input.charAt(++index);
	}

	/**
	 * Peek for the character at peekIndex. This is useful for parsing elements of
	 * which you can't be sure what type it is. For instance, 'A' can mean: the
	 * start of <code>Function</code> ADD(), or the <code>Reference</code> to
	 * <code>Cell</code> A1.
	 * 
	 * param n Amount of characters to peek forward
	 * @return The character at the given index
	 */
	private char peek() {
		if ( peekIndex < length ) {
			return input.charAt(peekIndex);
		}
		return 0;
	}

	/**
	 * Skips to the next character of the given value
	 * @param character Character to search for
	 */
	private void skipToNext(char character) {
		while(hasNext()) {
			if ( next() == character ) {
				break;
			}
		}
	}

	/**
	 * Method to peek for a word at current peekIndex, and
	 * update the current index if the input matches the needle.
	 * @param needle String to search for
	 * @return <code>true</code> if found
	 */
	private boolean searchWord(String needle) {
		peekIndex = index;
		for (;;) {
			char c = Character.toUpperCase(peek());
			int i = peekIndex - index;
			if ( i+1 == needle.length() ) {
				index = peekIndex;
				return true;
			} else if ( needle.charAt(i) == c) {
				peekIndex++;
				continue;
			} else {
				return false;
			}
		}
	}

	/**
	 * Get the previous character in the <code>String</code>.
	 * 
	 * @return The previous character, or <code>0</code> if the current
	 *         <code>index</code> is <code>0</code>.
	 */
	private char previous() {
		if (index > 0) {
			return input.charAt(index - 1);
		}
		return 0;
	}

	@Override
	public String toString() {
		return input;
	}

	/**
	 * Static method to initiate parsing of an expression. Automatically slices
	 * of the '=' character at the beginning, and removes spaces.
	 * 
	 * @param string
	 *            Input
	 * @return The result of the parse, which can be of various types. When the
	 *         input <code>String</code> did not start with a <code>=</code>
	 *         character, the original <code>String</code> is returned.
	 */
	public static Object parse(Sheet sheet, String string) {
		if (string.length() < 2 || string.charAt(0) != '=') {
			return string;
		} else {
			return new Parser(sheet, string.substring(1).replaceAll("\\s+", "")).parse();
		}
	}
	
	public static Object parse(Cell cell) {
		return new Parser(cell).parse();
	}

}
