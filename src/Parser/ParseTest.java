package Parser;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import File.Cell;
import File.Sheet;

public class ParseTest {

	public Sheet sheet;

	public Cell A1;
	public Cell A2;
	public Cell A3;
	public Cell B1;
	public Cell B2;
	public Cell B3;

	@Before
	public void setUp() throws Exception {
		sheet = new Sheet();
		A1 = sheet.createCell("bliep", 0, 0);
		A2 = sheet.createCell("5", 0, 1);
		A3 = sheet.createCell("=5", 0, 2);
		B1 = sheet.createCell("=5*2", 1, 0);
		B2 = sheet.createCell("=2+2*3", 1, 1);
		B3 = sheet.createCell("=ADD(5,3)", 1, 2);
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
		assertEquals("Bert is de sigaar",
				Parser.parse(sheet, "Bert is de sigaar"));
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
		assertEquals(7, Parser.parse(sheet, "=ADD(3,4)"));
	}

	@Test
	public void simpleFunctionWithCast() {
		assertEquals(7, Parser.parse(sheet, "=ADD(3,4.1234)"));
	}

	@Test
	public void nestedFunction() {
		assertEquals(6, Parser.parse(sheet, "=ADD(3,ADD(1,2))"));
	}

	@Test
	public void testMultiAddParseIntTrue() {
		assertEquals(10, Parser.parse(sheet, "=ADD(ADD(1,2),ADD(3,4))"));
	}

	@Test
	public void functionWithSpaces() {
		assertEquals(6, Parser.parse(sheet, "=ADD ( 3, A D D ( 1 , 2 ) ) "));
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
		assertEquals(6, Parser.parse(sheet, "=3+3.2"));
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
		assertEquals(3, Parser.parse(sheet, "=5-2.8"));
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
		assertEquals(13, Parser.parse(sheet, "=ADD(5,2)+3.0*2"));
	}

	@Test
	public void twoFunctions() {
		assertEquals(10, Parser.parse(sheet, "=ADD(1,2)+ADD(3,4)"));
	}

	@Test
	public void simpleReference() {
		assertEquals(A1.getInput(), Parser.parse(sheet, "=A1"));
	}

	@Test
	public void simpleRange() {
		assertEquals(26, Parser.parse(sheet, "=ADD(B1:B3)"));
	}
}
