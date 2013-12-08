package Parser;

import static org.junit.Assert.*;

import org.junit.Test;

public class ParseTest {
	
	@Test
	public void noCalculation() {
		assertEquals("Bert is de sigaar", Parser.parse("Bert is de sigaar"));
	}
	
	@Test
	public void integerValue() {
		assertEquals(5, Parser.parse("=5"));
	}
	
	@Test
	public void biggerIntegerValue() {
		assertEquals(12345, Parser.parse("=12345"));
	}
	
	@Test
	public void doubleValue() {
		assertEquals(5.1234, Parser.parse("=5.1234"));
	}
	
	@Test
	public void doubleENotation() {
		assertEquals(5e3, Parser.parse("=5e3"));
	}
	
	@Test
	public void simpleFunction() {
		assertEquals(7, Parser.parse("=ADD(3,4)"));
	}
	
	@Test
	public void simpleFunctionWithCast() {
		assertEquals(7, Parser.parse("=ADD(3,4.1234)"));
	}
	
	@Test
	public void nestedFunction() {
		assertEquals(6, Parser.parse("=ADD(3,ADD(1,2))"));
	}
	
	
	@Test
	public void testMultiAddParseIntTrue() {
		assertEquals(10, Parser.parse("=ADD(ADD(1,2),ADD(3,4))"));
	}
	
	@Test
	public void functionWithSpaces() {
		assertEquals(6, Parser.parse("=ADD ( 3, A D D ( 1 , 2 ) ) "));
	}
	
	@Test
	public void plusOperator() {
		assertEquals(6, Parser.parse("=3+3"));
	}

	@Test
	public void plusOperatorDouble() {
		assertEquals(6.2, Parser.parse("=3.0+3.2"));
	}
	
	@Test
	public void plusOperatorDoubleAndInt() {
		assertEquals(6.2, Parser.parse("=3.2+3"));
	}
	
	@Test
	public void plusOperatorIntAndDouble() {
		assertEquals(6, Parser.parse("=3+3.2"));
	}

	@Test
	public void addPositiveToNegative() {
		assertEquals(3, Parser.parse("=-2+5"));
	}
	
	@Test
	public void addNegativeToPositive() {
		assertEquals(-3, Parser.parse("=2+-5"));
	}
	
	@Test
	public void subtractOperator() {
		assertEquals(2, Parser.parse("=5-3"));
	}
	
	@Test
	public void subtractOperatorNegative() {
		assertEquals(-2, Parser.parse("=3-5"));
	}
		
	@Test
	public void subtractOperatorDouble() {
		assertEquals(2.2, Parser.parse("=5.2-3.0"));
	}
	
	@Test
	public void subtractOperatorDoubleAndInt() {
		assertEquals(2.2, Parser.parse("=5.2-3"));
	}
	
	@Test
	public void subtractOperatorIntAndDouble() {
		assertEquals(3, Parser.parse("=5-2.8"));
	}

	@Test
	public void multiplyOperator() {
		assertEquals(6, Parser.parse("=2*3"));
	}
	
	@Test
	public void divideOperator() {
		assertEquals(2, Parser.parse("=6/3"));
	}

	@Test
	public void testMultiplyOverPlus() {
		assertEquals(8, Parser.parse("=2+2*3"));
	}

	@Test
	public void testLeftToRight() {
		assertEquals(20, Parser.parse("=8/2*5"));
	}
	
	@Test
	public void testBrackets() {
		assertEquals(18, Parser.parse("=3*(2+4)"));
	}
	
	public void testComplex() {
		assertEquals(11, Parser.parse("=5+2*3"));
	}
	
	@Test
	public void testCombined() {
		assertEquals(13, Parser.parse("=ADD(5,2)+3.0*2"));
	}

	@Test
	public void twoFunctions() {
		assertEquals(10, Parser.parse("=ADD(1,2)+ADD(3,4)"));
	}
}
