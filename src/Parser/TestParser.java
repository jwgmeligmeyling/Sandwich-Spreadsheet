package Parser;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import File.Cell;
import File.Sheet;

public class TestParser {

	public Sheet sheet;
	public Cell A1, A2, A3, B1, B2, B3;

	@Before
	public void setUp() throws Exception {
		sheet = new Sheet();
		A1 = sheet.createCell("bliep", 0, 0);
		A2 = sheet.createCell("5", 0, 1);
		A3 = sheet.createCell("=5", 0, 2);
		B1 = sheet.createCell("=5*2", 1, 0);
		B2 = sheet.createCell("=2+2*3", 1, 1);
		B3 = sheet.createCell("=SUM(5,3)", 1, 2);
		sheet.init();
	}

	@Test
	public void cellParsingString() {
		assertEquals("bliep", A1.getValue());
	}

	@Test
	public void cellParsingNumber() {
		assertEquals(5, A3.getValue());
	}

	@Test
	public void cellParsingOperator() {
		assertEquals(8, B2.getValue());
	}

	@Test
	public void cellParsingFunction() {
		assertEquals(8, B3.getValue());
	}

	@Test
	public void noCalculation() {
		assertEquals("Bert is de sigaar", Parser.parse(sheet, "Bert is de sigaar"));
	}

	@Test
	public void integerValue() {
		assertEquals(5, Parser.parse(sheet, "=5"));
	}

	@Test
	public void biggerIntegerValue() {
		assertEquals(12345, Parser.parse(sheet, "=12345"));
	}

	@Test
	public void doubleValue() {
		assertEquals(5.1234, ((Double) Parser.parse(sheet, "=5.1234")).doubleValue(), 1e-15);
	}

	@Test
	public void doubleENotation() {
		assertEquals(5e3, ((Double)  Parser.parse(sheet, "=5e3")).doubleValue(), 1e-15);
	}

	@Test
	public void simpleFunction() {
		assertEquals(7, Parser.parse(sheet, "=SUM(3,4)"));
	}

	@Test
	public void simpleFunctionWithCast() {
		assertEquals(7.1234, Parser.parse(sheet, "=SUM(3,4.1234)"));
	}

	@Test
	public void nestedFunction() {
		assertEquals(6, Parser.parse(sheet, "=SUM(3,SUM(1,2))"));
	}

	@Test
	public void testMultiAddParseIntTrue() {
		assertEquals(10, Parser.parse(sheet, "=SUM(SUM(1,2),SUM(3,4))"));
	}

	@Test
	public void functionWithSpaces() {
		assertEquals(6, Parser.parse(sheet, "=SUM ( 3, S U M ( 1 , 2 ) ) "));
	}

	@Test
	public void plusOperator() {
		assertEquals(6, Parser.parse(sheet, "=3+3"));
	}

	@Test
	public void plusOperatorDouble() {
		assertEquals(6.2, ((Double) Parser.parse(sheet, "=3.0+3.2")).doubleValue(), 1e-15);
	}

	@Test
	public void plusOperatorDoubleAndInt() {
		assertEquals(6.2, ((Double)  Parser.parse(sheet, "=3.2+3")).doubleValue(), 1e-15);
	}

	@Test
	public void plusOperatorIntAndDouble() {
		assertEquals(6.2, Parser.parse(sheet, "=3+3.2"));
	}

	@Test
	public void addPositiveToNegative() {
		assertEquals(3, Parser.parse(sheet, "=-2+5"));
	}

	@Test
	public void addNegativeToPositive() {
		assertEquals(-3, Parser.parse(sheet, "=2+-5"));
	}

	@Test
	public void subtractOperator() {
		assertEquals(2, Parser.parse(sheet, "=5-3"));
	}

	@Test
	public void subtractOperatorNegative() {
		assertEquals(-2, Parser.parse(sheet, "=3-5"));
	}

	@Test
	public void subtractOperatorDouble() {
		assertEquals(2.2, ((Double)  Parser.parse(sheet, "=5.2-3.0")).doubleValue(), 1e-15);
	}

	@Test
	public void subtractOperatorDoubleAndInt() {
		assertEquals(2.2, ((Double) Parser.parse(sheet, "=5.2-3")).doubleValue(), 1e-15);
	}

	@Test
	public void subtractOperatorIntAndDouble() {
		assertEquals(2.2, Parser.parse(sheet, "=5-2.8"));
	}

	@Test
	public void multiplyOperator() {
		assertEquals(6, Parser.parse(sheet, "=2*3"));
	}

	@Test
	public void divideOperator() {
		assertEquals(2, Parser.parse(sheet, "=6/3"));
	}

	@Test
	public void testMultiplyOverPlus() {
		assertEquals(8, Parser.parse(sheet, "=2+2*3"));
	}

	@Test
	public void testLeftToRight() {
		assertEquals(20, Parser.parse(sheet, "=8/2*5"));
	}

	@Test
	public void testBrackets() {
		assertEquals(18, Parser.parse(sheet, "=3*(2+4)"));
	}

	public void testComplex() {
		assertEquals(11, Parser.parse(sheet, "=5+2*3"));
	}

	@Test
	public void testCombined() {
		assertEquals(13, Parser.parse(sheet, "=SUM(5,2)+3.0*2"));
	}

	@Test
	public void twoFunctions() {
		assertEquals(10, Parser.parse(sheet, "=SUM(1,2)+SUM(3,4)"));
	}

	@Test
	public void simpleReference() {
		assertEquals(A1.getInput(), Parser.parse(sheet, "=A1"));
	}

	@Test
	public void simpleRange() {
		assertEquals(26, Parser.parse(sheet, "=SUM(B1:B3)"));
	}
	

	@Test
	public void multiplySingleReferences() {
		assertEquals(80, Parser.parse(sheet, "=B1*B3"));
	}
	
	@Test
	public void testNullReference() {
		Parser.parse(sheet, "=SUM(B1:B4)");
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testInvalidReference() {
		Parser.parse(sheet, "=SUM(B1:)");
	}
	

	@Test(expected=IllegalArgumentException.class)
	public void testInvalidFunction() {
		Parser.parse(sheet, "=UNDEFINED(B1)");
	}
	

	@Test(expected=IllegalArgumentException.class)
	public void testInvalidOperator() {
		Parser.parse(sheet, "=B1|&B2");
	}
	
	@Test
	public void concatStrings() {
		assertEquals("BLIEPBLA", Parser.parse(sheet, "='BLIEP'&'BLA'"));
	}
	
	@Test
	public void fairlyComplexFunction() {
		assertEquals(68, Parser.parse(sheet, "=2+3^2*4+3*7+(2+1)*3"));
	}
	
	@Test
	public void testToString() {
		Parser p = new Parser(sheet, "SUM(5,3)");
		assertEquals("SUM(5,3)", p.toString());
	}
	
	@Test
	public void testWithDoubleOperator() {
		assertEquals(16, Parser.parse(sheet, "=2<<3"));
	}
	
	@Test
	public void testNegativeFunction() {
		assertEquals(-5, Parser.parse(sheet, "=-SUM(3,2)"));
	}
	
	@Test
	public void testENotation() {
		assertEquals(5e3, Parser.parse(sheet, "=5e3"));
		assertEquals(5e3, Parser.parse(sheet, "=5E3"));
	}
	
	@Test
	public void strInBrackets() {
		assertEquals("SDAFBAAB", Parser.parse(sheet, "='SDAF'&('BA'&'AB')"));
	}
	

	@Test
	public void strInQuotes() {
		assertEquals("sadfsadf", Parser.parse(sheet, "=\"sadfsadf\""));
	}
	
	@Test
	public void realDeep() {
		assertEquals(1+2+3+4+5+6+7, Parser.parse(sheet, "=(((((SUM(1,2))+3)+4)+5)+6)+7"));
	}
	
	@Test
	public void parseNullSheet() {
		assertEquals(3, Parser.parse(null, "=SUBTRACT(10,7)"));
	}
	
	@Test(expected=NullPointerException.class)
	public void parseNullString() {
		assertEquals(3, Parser.parse(sheet, null));
	}

	@Test
	public void testBoolean() {
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=TRUE"));
	}
	
	@Test
	public void testBooleanLcase() {
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true"));
	}
	
	@Test
	public void testBooleanFalse() {
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=false"));
	}
	
	@Test
	public void testBooleanOperatorWithBooles() {
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=false&&true"));
	}
	
	@Test
	public void testBooleanOperatorWithBooles1() {
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true&&true"));
	}

	@Test
	public void testBooleanOperatorWithBooles2() {
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=false||true"));
	}
	
	/**
	 * For each pair of parentheses 2 calls are pushed to the Stack, one
	 * for {@link Parser.Parser#closeBracket closeBracket()} and one for
	 * {@link Parser.Parser#parse()}. This method checks how deep you can
	 * go with parentheses. Jan-Willem has succeeded to run this test with
	 * a depth of 7400 parentheses (+/- 15000 calls) which ran in 0.390 s.
	 */
	@Test
	public void testParenthesesDepth() {
		int depth = 7400;
		StringBuilder query = new StringBuilder(depth*2+2);
		query.append('=');
	
		for ( int i = 0; i < depth; i++ ) 
			query.append('(');
		query.append('5');
		for ( int i = 0; i < depth; i++ )
			query.append(')');
		
		assertEquals(5, Parser.parse(sheet, query.toString()));
	}
	
	/**
	 * <p>This test concatenates some operators with 5, in the order such
	 * that the returned result should be five:</p>
	 * 
	 * <pre> =5...+5*5/5-5... == 5.</pre>
	 * 
	 * <p>This doesn't really pushes the {@code Operator Stack} to it's
	 * limit, because the +, -, / and * operators only have two
	 * different precedences, so the average amount of elements in
	 * both the value stack and the operator stack will be 2-3.</p>
	 * 
	 * <p>This test case doesn't really push the JVM stack as well, because
	 * there is no recursion (or parentheses) in this function. This
	 * test will just continue until the calculation is completed, or
	 * the system runs out of memory.</p>
	 * 
	 * <p>Amount  5.000.000 (20.000.000 operators) runs in  9.485 seconds
	 * Amount 10.000.000 (40.000.000 operators) runs in 17.717 seconds
	 * -> We see a pattern here and I'm not patient enough to test it
	 * with 100.000.000 runs.</p>
	 */
	@Test
	public void testOperatorLimit() {
		int amount = 10000000;
		StringBuilder query = new StringBuilder(amount*3);
		query.append('=');
		query.append('5');
		
		for ( int i = 0; i < amount; i++ ) {
			query.append('+');
			query.append('5');
			query.append('*');
			query.append('5');
			query.append('/');
			query.append('5');
			query.append('-');
			query.append('5');
		}
		
		assertEquals(5, Parser.parse(sheet, query.toString()));
	}
	
	/**
	 * This function creates a Parser input in the form of
	 * {@code =...SUM(SUM(SUM(5,5),5),5)...} . Jan-Willem
	 * tested this and could go to a depth of 14000 nested
	 * functions, which ran in 2.827s and 16000 nested
	 * functions which ran in 4.035 s.
	 * TODO Why can we go further with these parentheses
	 * while they use the same recursion as in the previous
	 * test case? Does the additional logic in the sum
	 * function give the JVM some time to optimize/cache the
	 * stack?
	 */
	@Test
	public void testFunctionRecursionDepth() {
		int depth = 16000;
		StringBuilder query = new StringBuilder(depth*7+2);
		query.append('=');
	
		// This creates a query string in the form of
		// =...SUM(SUM(SUM(5,5),5),5)...;
		
		for ( int i = 0; i < depth; i++ ) 
			query.append("SUM(");
		query.append("5");
		for ( int i = 0; i < depth; i++ )
			query.append(",5)");
		
		assertEquals(5*depth+5, Parser.parse(sheet, query.toString()));
	}
	
}
