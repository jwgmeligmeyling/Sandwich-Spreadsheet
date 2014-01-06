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
		C2 = sheet.createCell("=PRODUCT(3,5,2)", 1, 1);
		C3 = sheet.createCell("", 2, 2);
	}

	@Test
	public void testSum() {
		assertEquals(10, Function.SUM.calculate(5, 5));
	}

	@Test
	public void testSumDoubles() {
		assertEquals(7.3,
				((Double) Function.SUM.calculate(6.9, 0.4)).doubleValue(),
				1e-15);
	}

	@Test
	public void testSumIntAndDouble() {
		assertEquals(7.25, Function.SUM.calculate(6, 1.25));
	}

	@Test
	public void testSumDoubleAndInt() {
		assertEquals(7.125,
				((Double) Function.SUM.calculate(6.125, 1)).doubleValue(),
				1e-15);
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

	@Test(expected = IllegalArgumentException.class)
	public void testIntMoreArgs() {
		assertEquals(5, Function.INT.calculate(5.2, 6.9));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAverageNoArgs() {
		Function.AVERAGE.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCountNoArgs() {
		Function.COUNT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCountaNoArgs() {
		Function.COUNTA.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCountifNoArgs() {
		Function.COUNTIF.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIfNoArgs() {
		Function.IF.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIntNoArgs() {
		Function.INT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIslogicalNoArgs() {
		Function.ISLOGICAL.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsevenNoArgs() {
		Function.ISEVEN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsnumberNoArgs() {
		Function.ISNUMBER.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLowerNoArgs() {
		Function.LOWER.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMaxNoArgs() {
		Function.MAX.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMedianNoArgs() {
		Function.MEDIAN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMinNoArgs() {
		Function.MIN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testModNoArgs() {
		Function.MOD.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testNotNoArgs() {
		Function.NOT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOrNoArgs() {
		Function.OR.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPowerNoArgs() {
		Function.POWER.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testProductNoArgs() {
		Function.PRODUCT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testProperNoArgs() {
		Function.PROPER.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRounddownNoArgs() {
		Function.ROUNDDOWN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRoundupNoArgs() {
		Function.ROUNDUP.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSignNoArgs() {
		Function.SIGN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSqrtNoArgs() {
		Function.SQRT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSumNoArgs() {
		Function.SUM.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSumifNoArgs() {
		Function.SUMIF.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRoundNoArgs() {
		Function.ROUND.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAndNoArgs() {
		Function.AND.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRootNoArgs() {
		Function.ROOT.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRandbetweenNoArgs() {
		Function.RANDBETWEEN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCosNoArgs() {
		Function.COS.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSinNoArgs() {
		Function.SIN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testTanNoArgs() {
		Function.TAN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLogNoArgs() {
		Function.LOG.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLogbaseNoArgs() {
		Function.LOGBASE.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLnNoArgs() {
		Function.LN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAbsNoArgs() {
		Function.ABS.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAcosNoArgs() {
		Function.ACOS.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAsinNoArgs() {
		Function.ASIN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAtanNoArgs() {
		Function.ATAN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDegreeNoArgs() {
		Function.DEGREE.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRadianNoArgs() {
		Function.RADIAN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDivideNoArgs() {
		Function.DIVIDE.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testExpNoArgs() {
		Function.EXP.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsoddNoArgs() {
		Function.ISODD.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testUpperNoArgs() {
		Function.UPPER.calculate();
	}

	/*
	 * @Test public void testCount() { assertEquals(4, Parser.parse(sheet,
	 * "=COUNT(A1:B3)")); }
	 */
}