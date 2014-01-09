package Parser;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import File.*;
import File.Sheet.Range;

public class TestFunctions {

	public Sheet sheet;
	public Cell A1, A2, A3, B1, B2, B3, C1, C2, C3, D1, D2, D3;
	public Range r1, r2, r3, r4;

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
		C2 = sheet.createCell("=PRODUCT(3,5,2)", 2, 1);
		C3 = sheet.createCell("", 2, 2);
		D1 = sheet.createCell("TRUE", 3, 0);
		D2 = sheet.createCell("FALSE", 3, 1);
		D3 = sheet.createCell("true", 3, 2);
		
		r1 = sheet.getRange(A1, D3); // All types
		r2 = sheet.getRange(B1, C2); // Numbers
		r3 = sheet.getRange(D1, D3); // Booleans
		r4 = sheet.getRange(A2, A2); // Cell A2 ( =5);
		
		/*
		 * Jan-Willem:
		 * Initialize sheet such that values are calculated based on current input
		 */
		sheet.init();
	}
	
	@Test
	public void testSumRangeWithNumbers() {
		assertEquals(48, Function.SUM.calculate(r2));
	}
	
	@Test
	public void testsumRangeWithBooles() {
		assertEquals(2, Function.SUM.calculate(r3));
	}

	@Test
	public void testSumRangeWithStrings() {
		assertEquals(68, Function.SUM.calculate(r1));
	}
	
	@Test
	public void testSum() {
		assertEquals(10, Function.SUM.calculate(5, 5));
	}
	
	@Test
	public void testSubtract() {
		assertEquals(5, Function.SUBTRACT.calculate(8,3));
	}
	
	@Test
	public void testDivideInteger() {
		// We could expect 1 here (3/2 integer division, but Excel always divides as Double)
		assertEquals(1.5, ((Number) Function.DIVIDE.calculate(3, 2)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testDivideDouble() {
		assertEquals(2.5, ((Number) Function.DIVIDE.calculate(5.0, 2.0)).doubleValue(), 1e-15);
	}

	@Test
	public void testSumDoubles() {
		assertEquals(7.3, ((Double) Function.SUM.calculate(6.9, 0.4)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testPower() {
		assertEquals(8, Function.POWER.calculate(2, 3));
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

	@Test(expected = IllegalArgumentException.class)
	public void testIntMoreArgs() {
		Function.INT.calculate(5.2, 6.9);
	}
	
	@Test
	public void testIsLogicalBoolean() {
		assertEquals(true, Function.ISLOGICAL.calculate(true));
	}
	
	@Test
	public void testIsLogicalCell() {
		assertEquals(true, Function.ISLOGICAL.calculate(D2));
	}
	@Test
	public void testIsLogicalRange() {
		assertEquals(true, Function.ISLOGICAL.calculate(r3));
	}	
	
	@Test
	public void testAddCalculateNegativeInt() {
		assertEquals(-9, Function.SUM.calculateNegative(5, 4));
	}
	
	@Test
	public void testAddCalculateNegativeDouble() {
		assertEquals(-4.56, ((Number) Function.SUM.calculateNegative(2, 2.56)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testMod() {
		assertEquals(1, Function.MOD.calculate(3,2));
	}
	
	@Test
	public void testAverage() {
		assertEquals(16, Function.AVERAGE.calculate(r2));
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
	 * Test not-function methodes
	 */
	
	@Test
	public void testGetDescription() {
		assertEquals("Returns the sum of a set of values contained in a specified field on a query.", Function.SUM.getDescription());
	}
	
	/*
	 * Test valueOf methods
	 */
	
	// doubleValueOf()

	@Test
	public void testDoubleValueOfDouble() {
		assertEquals(5.0236, Function.doubleValueOf(5.0236), 0.00001);
	}

	@Test
	public void testDoubleValueOfInt() {
		assertEquals(6.0, Function.doubleValueOf(6), 0.1);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDoubleValueOfNull() {
		Function.doubleValueOf(null);
	}

	@Test
	public void testDoubleValueOfRange() {
		assertEquals(10.0, Function.doubleValueOf(r2), 0.01);
	}

	@Test
	public void testDoubleValueOfCell() {
		assertEquals(8.0, Function.doubleValueOf(B3), 0.01);
	}

	@Test
	public void testDoubleValueOfBooleanTrue() {
		assertEquals(1.0, Function.doubleValueOf(true), 0.01);
	}

	@Test
	public void testDoubleValueOfBooleanFalse() {
		assertEquals(0.0, Function.doubleValueOf(false), 0.01);
	}

	@Test
	public void testDoubleValueOfStringEmpty() {
		assertEquals(0.0, Function.doubleValueOf(""), 0.01);
	}

	@Test
	public void testDoubleValueOfString() {
		assertEquals(0.0, Function.doubleValueOf("This is a string"), 0.01);
	}

	// booleanValueOf()

	@Test
	public void testBooleanValueOfBool() {
		assertEquals(false, Function.booleanValueOf(false));
	}

	@Test
	public void testBooleanValueOfRangeTrue() {
		assertEquals(true, Function.booleanValueOf(D1));
	}

	@Test
	public void testBooleanValueOfRangeFalse() {
		assertEquals(false, Function.booleanValueOf(D2));
	}

	@Test
	public void testBooleanValueOfInt() {
		assertEquals(true, Function.booleanValueOf(1));
	}

	@Test
	public void testBooleanValueOfStringBoolFalse() {
		assertEquals(false, Function.booleanValueOf(0));
	}

	@Test
	public void testBooleanValueOfString() {
		assertEquals(false, Function.booleanValueOf(""));
	}

	@Test
	public void testBooleanValueOfStringBool() {
		assertEquals(true, Function.booleanValueOf("TRUE"));
	}
	
	@Test
	public void testBooleanValueOfNull() {
		assertEquals(false, Function.booleanValueOf(null));
	}
	
	@Test
	public void testBooleanValueOfRange() {
		assertEquals(true, Function.booleanValueOf(r3));
	}

	// intValueOf()

	@Test(expected = IllegalArgumentException.class)
	public void testIntValueOfString() {
		Function.intValueOf("hi");
	}

	@Test
	public void testIntValueOfInt() {
		assertEquals(5, Function.intValueOf(5));
	}

	@Test
	public void testIntValueOfBoolFalse() {
		assertEquals(0, Function.intValueOf(false));
	}

	@Test
	public void testIntValueOfBoolTrue() {
		assertEquals(1, Function.intValueOf(true));
	}
	
	@Test
	public void testIntValueOfRange() {
		assertEquals(10, Function.intValueOf(r2));
	}
	
	@Test
	public void testIntValueOfCell() {
		assertEquals(8, Function.intValueOf(B2));
	}
	
	// stringValueOf()
	
	@Test
	public void testStringValueOfString() {
		assertEquals("This is a string", Function.stringValueOf("This is a string"));
	}
	
	@Test
	public void testStringValueOfCell() {
		assertEquals("bliep", Function.stringValueOf(A1));
	}
	
	@Test
	public void testStringValueOfRange() {
		assertEquals("bliep", Function.stringValueOf(r1));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testStringValueOfNull() {
		Function.stringValueOf(null);
	}
	
	/*
	 * Test assert methods
	 */
	
	@Test(expected = IllegalArgumentException.class)
	public void testAssertMinArgumentsError() {
		Function.assertMinArguments(1, 0);
	}
	@Test
	public void testAssertMinArgumentsOk() {
		Function.assertMinArguments(1, 1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testAssertArgumentsError() {
		Function.assertArguments(0, 5);
	}
	@Test
	public void testAssertArgumentsOk() {
		Function.assertArguments(2, 2);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testAssertTwoArgumentsError() {
		Function.assertTwoArguments(1, 2, 0);
	}
	@Test
	public void testAssertTwoArgumentsOk1() {
		Function.assertTwoArguments(1, 2, 1);
	}
	@Test
	public void testAssertTwoArgumentsOk2() {
		Function.assertTwoArguments(1, 2, 2);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAssertRangeError1() {
		Function.assertArgumentRange(A1);
	}
	@Test
	public void testAssertRangeOk1() {
		Function.assertArgumentRange(r2);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAssertRangeError2() {
		Object[] args = new Object[2];
		args[0] = "this is a string";
		args[1] = r1;
		Function.assertArgumentRange(0, args);
	}
	@Test
	public void testAssertRangeOk2() {
		Object[] args = new Object[2];
		args[0] = "this is a string";
		args[1] = r1;
		Function.assertArgumentRange(1, args);
	}

	@Test
	public void testAssertSingleRangeOk1() {
		Function.assertArgumentSingleRange("this is a string, not a range nor a cell");
	}
	@Test
	public void testAssertSingleRangeOk2() {
		Function.assertArgumentSingleRange(B3);
	}
	@Test
	public void testAssertSingleRangeOk3() {
		Function.assertArgumentSingleRange(null);
	}
	@Test
	public void testAssertSingleRangeOk4() {
		Function.assertArgumentSingleRange(r4);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAssertSingleRangeError() {
		Function.assertArgumentSingleRange(r1);
	}
}