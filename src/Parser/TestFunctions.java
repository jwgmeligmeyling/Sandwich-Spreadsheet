package Parser;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import File.*;

public class TestFunctions {
	
	public Sheet sheet;
	public Cell A1, A2, A3, B1, B2, B3, C1, C2, C3;

	@Before
	public void setUp() throws Exception {
		sheet = new Sheet();
		A1 = sheet.createCell("bliep", 0, 0);
		A2 = sheet.createCell("5", 0, 1);
		A3 = sheet.createCell("=5", 0, 2);
		B1 = sheet.createCell("=5*2", 1, 0);
		B2 = sheet.createCell("=2+2*3", 1, 1);
		B3 = sheet.createCell("=SUM(5,3)", 1, 2);
		C1 = sheet.createCell("2+2", 2, 0);
		C2 = sheet.createCell("=PRODUCT(3,5,2)",1,1);
		C3 = sheet.createCell("", 2, 2);
	}
	
	@Test
	public void testSum() {
		assertEquals(10, Function.SUM.calculate(5, 5));
	}

	@Test
	public void testSumDoubles() {
		assertEquals(7.3, ((Double) Function.SUM.calculate(6.9, 0.4)).doubleValue(), 1e-15);
	}

	@Test
	public void testSumIntAndDouble() {
		assertEquals(7.25, Function.SUM.calculate(6, 1.25));
	}

	@Test
	public void testSumDoubleAndInt() {
		assertEquals(7.125, ((Double) Function.SUM.calculate(6.125, 1)).doubleValue(), 1e-15);
	}

	@Test
	public void testAndTrue() {
		assertTrue((Boolean) Function.AND.calculate(true, true));
	}

	@Test
	public void testAndFalse() {
		assertFalse((Boolean) Function.AND.calculate(true, false));
	}

	@Test
	public void testOrTrue() {
		assertTrue((Boolean) Function.OR.calculate(false, false, true));
	}

	@Test
	public void testOrFalse() {
		assertFalse((Boolean) Function.OR.calculate(false, false));
	}
	
	@Test
	public void testRoundToInt() {
		assertEquals(5, Function.ROUND.calculate(5.4589, 0));
	}
	
	@Test
	public void testRoundTo1DecPlace() {
		assertEquals(7.3, Function.ROUND.calculate(7.250001, 1));
	}
	
	@Test
	public void testIntPositive() {
		assertEquals(15, Function.INT.calculate(15.9));
	}
	
	@Test
	public void testIntNegative() {
		assertEquals(-9, Function.INT.calculate(-8.2));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIntMoreArgs() {
		assertEquals(5, Function.INT.calculate(5.2, 6.9));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIntNoArgs() {
		Function.INT.calculate();
	}
	
	@Test
	public void testCount() {
		assertEquals(5, Parser.parse(sheet, "=COUNT(A1:B3)"));
	}
}