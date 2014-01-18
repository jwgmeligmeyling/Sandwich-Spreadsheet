package Tests;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import File.Cell;
import File.Sheet;
import Parser.Parser;

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
	
	@Test
	public void testLogicOperators() {
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=2<=2"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=2<=3"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=2<=1"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=2>=2"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=3>=2"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=1>=2"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=3!=2"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=3<>2"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true!=false"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true<>false"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=true!=true"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=true<>true"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true||false"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true||true"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=false||false"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=false||true"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=true&&false"));
		assertEquals(Boolean.TRUE, Parser.parse(sheet, "=true&&true"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=false&&false"));
		assertEquals(Boolean.FALSE, Parser.parse(sheet, "=false&&true"));
		assertEquals(16, Parser.parse(sheet, "=2<<3"));
		assertEquals(2, Parser.parse(sheet, "=16>>3"));
		assertEquals(2, Parser.parse(sheet, "=2.1%4.4"));
		assertEquals(2, Parser.parse(sheet, "=2%5"));
	}
	
}
