package Tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import File.*;
import File.Sheet.Range;
import Parser.Function;

public class TestFunctions {

	public Sheet sheet, tsheet;
	public Cell A1, A2, A3, B1, B2, B3, C1, C2, C3, D1, D2, D3, E1, E2, E3;
	public Cell tA1, tA2, tA3, tA4, tA5, tB1, tB2, tB3, tB4, tB5, tC1, tC2, tC3, tC4, tC5;
	public Range r1, r2, r3, r4, r5, r6, r7;
	public Range tBools, tValS, tValB, tValBwrong;

	@Before
	public void setUp() throws Exception {
		
		Workbook workbook = new Workbook();
		Sheet sheet = workbook.createSheet();
		Sheet tsheet = workbook.createSheet();
		
		A1 = sheet.createCell("bliep", 0, 0);
		A2 = sheet.createCell("5", 0, 1);
		A3 = sheet.createCell("=5", 0, 2);
		
		B1 = sheet.createCell("=5*2", 1, 0);
		B2 = sheet.createCell("=2+2*3", 1, 1);
		B3 = sheet.createCell("=SUM(5,3)", 1, 2);
		
		C1 = sheet.createCell("=2+2", 2, 0);
		C2 = sheet.createCell("=PRODUCT(3,5,2)", 2, 1);
		C3 = sheet.createCell("", 2, 2);
		
		D1 = sheet.createCell("TRUE", 3, 0);
		D2 = sheet.createCell("FALSE", 3, 1);
		D3 = sheet.createCell("true", 3, 2);
		
		E1 = sheet.createCell("2.65", 4, 0);
		E2 = sheet.createCell("0", 4, 1);
		E3 = sheet.createCell("-3.2", 4, 2);
		
		r1 = sheet.getRange(A1, D3); // All types
		r2 = sheet.getRange(B1, C2); // Numbers
		r3 = sheet.getRange(D1, D3); // Booleans
		r4 = sheet.getRange(A2, A2); // Cell A2 (=5)
		r5 = sheet.getRange(A2, C2); // Numbers
		r6 = sheet.getRange(E1, E3); // Doubles
		r7 = sheet.getRange(A1, E1); // Row (for Match function)
		
		tA1 = tsheet.createCell("true", 0, 0);
		tA2 = tsheet.createCell("false", 0, 1);
		tA3 = tsheet.createCell("true", 0, 2);
		tA4 = tsheet.createCell("true", 0, 3);
		tA5 = tsheet.createCell("false", 0, 4);

		tB1 = tsheet.createCell("5", 1, 0);
		tB2 = tsheet.createCell("-2", 1, 1);
		tB3 = tsheet.createCell("4", 1, 2);
		tB4 = tsheet.createCell("8", 1, 3);
		tB5 = tsheet.createCell("3.5", 1, 4);
		      
		tC1 = tsheet.createCell("20", 2, 0);
		tC2 = tsheet.createCell("78", 2, 1);
		tC3 = tsheet.createCell("2.05", 2, 2);
		tC4 = tsheet.createCell("16", 2, 3);
		tC5 = tsheet.createCell("-5", 2, 4);
		
		tBools = tsheet.getRange(tA1, tA5);
		tValS = tsheet.getRange(tB1, tB5);
		tValB = tsheet.getRange(tC1, tC5);
		tValBwrong = tsheet.getRange(tC1, tC4);
		
		/*
		 * Jan-Willem:
		 * Initialize sheet such that values are calculated based on current input
		 */
		sheet.init();
		tsheet.init();
	}
	
	@Test
	public void testSubtract() {
		assertEquals(5, Function.SUBTRACT.calculate(8,3));
	}
	
	@Test
	public void testDivideInteger() {
		assertEquals(1.5, ((Number) Function.DIVIDE.calculate(3, 2)).doubleValue(), 1e-15);
	}
	@Test
	public void testDivideDouble() {
		assertEquals(2.5, ((Number) Function.DIVIDE.calculate(5.0, 2.0)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testPower() {
		assertEquals(8, Function.POWER.calculate(2, 3));
	}

	@Test
	public void testSumRangeNumber() {
		assertEquals(52, Function.SUM.calculate(r2));
	}
	
	@Test
	public void testSumRangeBoolean() {
		assertEquals(2, Function.SUM.calculate(r3));
	}
	
	@Test
	public void testSumRangeWithString() {
		assertEquals(72, Function.SUM.calculate(r1));
	}
	
	@Test
	public void testSum() {
		assertEquals(10, Function.SUM.calculate(5, 5));
	}
	
	@Test
	public void testSumDouble() {
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
	public void testSumIf2args() {
		assertEquals(114, Function.SUMIF.calculate(tValB, ">10"));
	}
	
	@Test
	public void testSumIf3args1() {
		assertEquals(38.05, Function.SUMIF.calculate(tBools, true, tValB));
	}
	
	@Test
	public void testSimIg3args2() {
		assertEquals(75.05, Function.SUMIF.calculate(tValS, "<5", tValB));
	}
	
	@Test
	public void testSumIf3argsNegative() {
		assertEquals(78, Function.SUMIF.calculate(tValS, -2, tValB));
	}
	
	@Test
	public void testSumIf3argsPositive() {
		assertEquals(16, Function.SUMIF.calculate(tValS, "8", tValB));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testSumIf3argsDiffRngSize() {
		Function.SUMIF.calculate(tValS, ">0", tValBwrong);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCount() {
		assertEquals(1, Function.COUNT.calculate(1, "string", "1"));
	}
	@Test
	public void testCountBoolean() {
		assertEquals(0, Function.COUNT.calculate(r3));
	}
	@Test
	public void testCountVarious() {
		assertEquals(7, Function.COUNT.calculate(r1));
	}
	
	@Test
	public void testCountA() {
		assertEquals(11, Function.COUNTA.calculate(r1));
	}

	@Test
	public void testCountIf1() {
		assertEquals(1, Function.COUNTIF.calculate(r3, false));
	}

	@Test
	public void testCountIf2() {
		assertEquals(3, Function.COUNTIF.calculate(r2, ">6"));
	}
	@Test
	public void testCountIfNoEquals() {
		assertEquals(1, Function.COUNTIF.calculate(r2, "8"));
	}
	@Test
	public void testCountIfSingleEquals() {
		assertEquals(1, Function.COUNTIF.calculate(r2, "=8"));
	}
	@Test
	public void testCountIfDoubleEquals() {
		assertEquals(1, Function.COUNTIF.calculate(r2, "==8"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testCountIfNoCriteria() {
		Function.COUNTIF.calculate();
	}

	@Test
	public void testIndex1() {
		assertEquals(20, Function.INDEX.calculate(tValB, 1, 1));
	}
	@Test
	public void testIndex2() {
		// TODO: check deze methode.
		assertEquals(-5, Integer.parseInt((String) Function.INDEX.calculate(tValB, 5, 1)));
	}
	@Test(expected = IllegalArgumentException.class)
	public void testIndexError1() {
		Function.INDEX.calculate(tValB, 0, 1);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testIndexError2() {
		Function.INDEX.calculate(tValB, 2, -1);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testIndexError3() {
		Function.INDEX.calculate(tValB, 6, 1);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testIndexError4() {
		Function.INDEX.calculate(tValB, 4, 2);
	}
	
	@Test
	public void testVLookup1() {
		assertEquals("bliep", Function.VLOOKUP.calculate("bliep", r1, 1));
	}
	@Test
	public void testVLookup2() {
		assertEquals(10, Function.VLOOKUP.calculate(A1, r1, 2));
	}
	@Test
	public void testVLookup3() {
		assertEquals(false, Function.VLOOKUP.calculate("5", r1, 4));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testVLookupError1() {
		Function.VLOOKUP.calculate(5, r1, 0);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testVLookupError2() {
		Function.VLOOKUP.calculate("bliep", r1, 5);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testVLookupError3() {
		Function.VLOOKUP.calculate("niet vindbaar", r1, 1);
	}
	
	@Test
	public void testMatchNumber() {
		assertEquals(4, Function.MATCH.calculate(8, tValS));
	}
	@Test
	public void testMatchMultiple1() {
		assertEquals(1, Function.MATCH.calculate(true, tBools));
	}
	@Test
	public void testMatchMultiple2() {
		assertEquals(2, Function.MATCH.calculate(false, tBools));
	}
	@Test
	public void testMatchString() {
		assertEquals(1, Function.MATCH.calculate("bliep", r7));
	}
	@Test(expected = IllegalArgumentException.class)
	public void testMatchErrorSize1() {
		Function.MATCH.calculate("maakt niet uit", r1);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testMatchErrorSize2() {
		Function.MATCH.calculate("maakt niet uit", r7);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testMatchErrorNotFound() {
		Function.MATCH.calculate("dit bestaat niet", tValB);
	}
	
	@Test
	public void testAdres() {
		assertEquals("D7", Function.ADRES.calculate(4, 7));
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdresNoArgs() {
		Function.ADRES.calculate();
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdres1Arg() {
		Function.ADRES.calculate(1);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdresInvalidArg1() {
		Function.ADRES.calculate(0,5);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdresInvalidArg2() {
		Function.ADRES.calculate(5,0);
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdresString() {
		Function.ADRES.calculate("hoi", "oei");
	}
	@Test(expected = IllegalArgumentException.class)
	public void testAdresChar() {
		Function.ADRES.calculate('v', 'c');
	}
	
	
	@Test
	public void testRoundUp1() {
		assertEquals(6, Function.ROUNDUP.calculate(5.2));
	}
	@Test
	public void testRoundUp2() {
		assertEquals(-6.9, Function.ROUNDUP.calculate(-6.82, 1));
	}
	@Test
	public void testRoundUp3() {
		assertEquals(100, Function.ROUNDUP.calculate(2, -2));
	}
	@Test
	public void testRoundUp4() {
		assertEquals(6, Function.ROUNDUP.calculate(5.2, 0));
	}
	@Test
	public void testRoundUp5() {
		assertEquals(6, Function.ROUNDUP.calculate(5.8));
	}

	@Test
	public void testRoundDown1() {
		assertEquals(5, Function.ROUNDDOWN.calculate(5.99));
	}
	@Test
	public void testRoundDown2() {
		assertEquals(-6.8, Function.ROUNDDOWN.calculate(-6.88, 1));
	}
	@Test
	public void testRoundDown3() {
		assertEquals(50, Function.ROUNDDOWN.calculate(59, -1));
	}

	@Test
	public void testRoot27b3() {
		assertEquals(3, Function.ROOT.calculate(27, 3));
	}
	@Test
	public void testRoot64b2() {
		assertEquals(8, Function.ROOT.calculate(64.0, 2));
	}
	@Test
	public void testRoot64b4() {
		assertEquals(4, ((Double)Function.ROOT.calculate(64.0, 3)).doubleValue(), 1e-15);
	}
	@Test
	public void testSqrt9() {
		assertEquals(3, Function.SQRT.calculate(9));
	}
	@Test
	public void testSqrt64() {
		assertEquals(8, Function.SQRT.calculate(64));
	}
	@Test
	public void testSqrt144() {
		assertEquals(12, Function.SQRT.calculate(144));
	}
	@Test
	public void testSqrt0() {
		assertEquals(0, Function.SQRT.calculate(0));
	}
	@Test
	public void test1() {
		assertEquals(1, Function.SQRT.calculate(1));
	}
	@Test
	public void testSqrt_min1() {
		assertEquals(Double.NaN, Function.SQRT.calculate(-1));
	}

	@Test
	public void testTan45grad() {
		assertEquals(1.0, ((Double)Function.TAN.calculate(0.25 * Math.PI)).doubleValue(), 1e-6);
	}
	@Test
	public void testTan0grad() {
		assertEquals(0.0, ((Double)Function.TAN.calculate(0)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testCos90grad() {
		assertEquals(0.0, ((Double)Function.COS.calculate(0.5 * Math.PI)).doubleValue(), 1e-6);
	}
	@Test
	public void testCos0grad() {
		assertEquals(1.0, ((Double)Function.COS.calculate(0)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testSin90grad() {
		assertEquals(1.0, ((Double)Function.SIN.calculate(0.5 * Math.PI)).doubleValue(), 1e-6);
	}
	@Test
	public void testSin0grad() {
		assertEquals(0, ((Double)Function.SIN.calculate(0)).doubleValue(), 1e-15);
	}
	
	@Test
	public void testASin90rad() {
		assertEquals(Double.NaN, ((Double)Function.ASIN.calculate(0.5 * Math.PI)).doubleValue(), 1e-6);
	}
	@Test
	public void testACos90rad() {
		assertEquals(Double.NaN, ((Double)Function.ACOS.calculate(0.5 * Math.PI)).doubleValue(), 1e-6);
	}
	@Test
	public void testATan1() {
		assertEquals(0.25 * Math.PI, ((Double)Function.ATAN.calculate(1)).doubleValue(), 1e-6);
	}
	
	@Test
	public void testRadian0grad() {
		assertEquals(0.0, Function.RADIAN.calculate(0.0));
	}
	@Test
	public void testRadian45grad() {
		assertEquals(0.25 * Math.PI, Function.RADIAN.calculate(45));
	}
	@Test
	public void testRadian90grad() {
		assertEquals(0.5 * Math.PI, Function.RADIAN.calculate(90));
	}
	@Test
	public void testRadian180grad() {
		assertEquals(Math.PI, Function.RADIAN.calculate(180));
	}
	@Test
	public void testRadian270grad() {
		assertEquals(1.5 * Math.PI, Function.RADIAN.calculate(270));
	}
	@Test
	public void testRadian360grad() {
		assertEquals(2.0 * Math.PI, Function.RADIAN.calculate(360));
	}
	@Test
	public void testRadian405grad() {
		assertEquals(2.25 * Math.PI, Function.RADIAN.calculate(405));
	}
	
	@Test
	public void testDegree0rad() {
		assertEquals(0.0, ((Double)Function.DEGREE.calculate(0.0)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree0_25rad() {
		assertEquals(45.0, ((Double)Function.DEGREE.calculate(0.25 * Math.PI)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree0_5rad() {
		assertEquals(90.0, ((Double)Function.DEGREE.calculate(0.5 * Math.PI)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree1rad() {
		assertEquals(180.0, ((Double)Function.DEGREE.calculate(Math.PI)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree1_5rad() {
		assertEquals(270.0, ((Double)Function.DEGREE.calculate(1.5 * Math.PI)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree2rad() {
		assertEquals(360.0, ((Double)Function.DEGREE.calculate(2.0 * Math.PI)).doubleValue(), 1e-10);
	}
	@Test
	public void testDegree2_25rad() {
		assertEquals(405.0, ((Double)Function.DEGREE.calculate(2.25 * Math.PI)).doubleValue(), 1e-10);
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
	public void testRoundDecPlaceMissing() {
		assertEquals(5, Function.ROUND.calculate(5.4589));
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
	public void testLower() {
		assertEquals("mixed case string", Function.LOWER.calculate("MiXed CASe sTRIng"));
	}
	
	@Test
	public void testUpper() {
		assertEquals("MIXED CASE STRING", Function.UPPER.calculate("mixED caSe strING"));
	}
	
	@Test
	public void testProper() {
		assertEquals("This Is8Ni_Ce/Ly Done", Function.PROPER.calculate("thIS is8nI_cE/LY done"));
	}
	
	@Test
	public void testLength() {
		assertEquals(3, Function.LENGTH.calculate("Hoi"));
	}
	@Test
	public void testLengthNumber() {
		assertEquals(4, Function.LENGTH.calculate(E1));
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
	public void testIsLogicalRangeTrue() {
		assertEquals(true, Function.ISLOGICAL.calculate(r3));
	}	
	@Test
	public void testIsLogicalRangeFalse1() {
		assertEquals(false, Function.ISLOGICAL.calculate(r1));
	}	
	@Test
	public void testIsLogicalRangeFalse2() {
		assertEquals(false, Function.ISLOGICAL.calculate(r2));
	}
	@Test
	public void testIsLogicalOther() {
		assertEquals(false, Function.ISLOGICAL.calculate("string"));
	}

	@Test
	public void testNotBooleanTrue() {
		assertEquals(false, Function.NOT.calculate(true));
	}
	@Test
	public void testNotBooleanFalse() {
		assertEquals(true, Function.NOT.calculate(false));
	}
	@Test
	public void testNotCell() {
		assertEquals(true, Function.NOT.calculate(D2));
	}
	@Test
	public void testNotRange() {
		assertEquals(false, Function.NOT.calculate(r3));
	}
	@Test
	public void testNotString() {
		assertEquals(false, Function.NOT.calculate("string"));
	}

	@Test
	public void testIfTrue() {
		assertEquals("this is true", Function.IF.calculate(true, "this is true", "nope"));
	}
	@Test
	public void testIfFalse() {
		assertEquals("nope", Function.IF.calculate(false, "this is true", "nope"));
	}
	@Test
	public void testIfFalseNoValIfFalse() {
		assertEquals(false, Function.IF.calculate(false, "this is true"));
	}
	@Test
	public void testIfTrueNoVals() {
		assertEquals(true, Function.IF.calculate(true));
	}
	@Test
	public void testIfFalseNoVals() {
		assertEquals(false, Function.IF.calculate(false));
	}
	
	@Test
	public void testMaxInt() {
		assertEquals(8, Function.MAX.calculate(5, 5, 6, 0, -10, 8));
	}
	@Test
	public void testMaxDouble() {
		assertEquals(6.7, Function.MAX.calculate(-254.02355, 6.7, 2, 5.55));
	}
	@Test
	public void testMaxIntOnlyDoubles() {
		assertEquals(6, Function.MAX.calculate(3.5, 6.0, 5.0, 4.2));
	}
	@Test
	public void testMaxNegative() {
		assertEquals(-1, Function.MAX.calculate(-5, -1, -100));
	}
	@Test
	public void testMaxString() {
		assertEquals(125, Function.MAX.calculate("string", 50.2, 125, 100));
	}
	@Test
	public void testMaxRange() {
		assertEquals(2.65, Function.MAX.calculate(r6));
	}
	@Test
	public void testMaxCellString() {
		assertEquals(5, Function.MAX.calculate(A1, A2));
	}
	@Test
	public void testMaxCellInt() {
		assertEquals(8, Function.MAX.calculate(B2, A2));
	}
	@Test
	public void testMaxMultipleRange() {
		assertEquals(30, Function.MAX.calculate(r5, r6));
	}
	
	@Test
	public void testMinInt() {
		assertEquals(-10, Function.MIN.calculate(5, 20, 6, 0, -10, 8));
	}
	@Test
	public void testMinDouble() {
		assertEquals(-254.02355, Function.MIN.calculate(-254.02355, 6.7, 2, 5.55));
	}
	@Test
	public void testMinIntOnlyDoubles() {
		assertEquals(3, Function.MIN.calculate(3.0, 6.0, 5.0, 4.2));
	}
	@Test
	public void testMinString() {
		assertEquals(50, Function.MIN.calculate(50, "hi", A1, 100.2));
	}
	@Test
	public void testMinCellInt() {
		assertEquals(8, Function.MIN.calculate(B1, B2, B3));
	}
	@Test
	public void testMinOnlyStrings() {
		assertEquals(0, Function.MIN.calculate("string1", A1));
	}
	@Test
	public void testMinStringFirst() {
		assertEquals(50, Function.MIN.calculate("hi", 50, 125));
	}
	@Test
	public void testMinRange() {
		assertEquals(5, Function.MIN.calculate("hi", r5));
	}
	@Test
	public void testMinMultipleRange() {
		assertEquals(0, Function.MIN.calculate(r6, r5));
	}
	@Test
	public void testMinNoNumbers() {
		assertEquals(50, Function.MIN.calculate("hi", 50, 125));
	}

	@Test
	public void testAbsIntPos() {
		assertEquals(50, Function.ABS.calculate(50));
	}
	@Test
	public void testAbsIntNeg() {
		assertEquals(50, Function.ABS.calculate(-50));
	}
	@Test
	public void testAbsIntZero() {
		assertEquals(0, Function.ABS.calculate(0));
	}
	@Test
	public void testAbsDoublePos() {
		assertEquals(21.65, Function.ABS.calculate(21.65));
	}
	@Test
	public void testAbsDoubleNeg() {
		assertEquals(21.65, Function.ABS.calculate(-21.65));
	}
	@Test(expected=NumberFormatException.class)
	public void testAbsString() {
		Function.ABS.calculate("string");
	}
	
	@Test
	public void testSignPos() {
		assertEquals(1, Function.SIGN.calculate(25.2));
	}
	
	@Test
	public void testSignNeg() {
		assertEquals(-1, Function.SIGN.calculate(-5));
	}
	
	@Test(expected=NumberFormatException.class)
	public void testSignString() {
		Function.SIGN.calculate("string");
	}
	
	@Test
	public void testSignZero() {
		assertEquals(0, Function.SIGN.calculate(0));
	}
	
	@Test
	public void testMod() {
		assertEquals(1, Function.MOD.calculate(3,2));
	}
	
	@Test
	public void testAverage() {
		assertEquals(13, Function.AVERAGE.calculate(r2));
	}
	
	@Test
	public void testLnE() {
		assertEquals(1, Function.LN.calculate(Math.E));
	}
	@Test
	public void testLnE2() {
		assertEquals(2, Function.LN.calculate(Math.pow(Math.E, 2)));
	}
	
	@Test
	public void testLog10w10() {
		assertEquals(1, Function.LOG.calculate(10));
	}
	@Test
	public void testLog10w1() {
		assertEquals(0, Function.LOG.calculate(1));
	}
	
	@Test
	public void testLogBase2v8() {
		assertEquals(3, Function.LOGBASE.calculate(8, 2));
	}
	@Test
	public void testLogBase4v1() {
		assertEquals(0, Function.LOGBASE.calculate(1, 4));
	}
	
	@Test
	public void testExpNoArgs() {
		assertEquals(Math.E, ((Number) Function.EXP.calculate()).doubleValue() , 1e-10);
	}
	@Test
	public void testExp1() {
		assertEquals(Math.E, ((Number) Function.EXP.calculate(1)).doubleValue() , 1e-10);
	}
	@Test
	public void testExp0() {
		assertEquals(1, Function.EXP.calculate(0));
	}
	@Test
	public void testExp5() {
		assertEquals(148.4131591, ((Number) Function.EXP.calculate(5)).doubleValue(), 1e-5);
	}
	
	@Test
	public void testPi() {
		assertEquals(Math.PI, Function.PI.calculate());
	}
	
	@Test
	public void testRand() {
		for(int i = 0; i < 1000; i++) {
			double temp = ((Double)Function.RAND.calculate()).doubleValue();
			assertTrue(temp >= 0 && temp <= 1);
		}
	}

	@Test
	public void testRandBetween0and5() {
		for(int i = 0; i < 1000; i++) {
			int temp = ((Number)Function.RANDBETWEEN.calculate(0, 5)).intValue();
			assertTrue(temp >= 0 && temp <= 5);
		}
	}
	@Test
	public void testRandBetween7and7() {
		for(int i = 0; i < 1000; i++) {
			int temp = ((Number)Function.RANDBETWEEN.calculate(7, 7)).intValue();
			assertTrue(temp == 7);
		}
	}
	@Test
	public void testRandBetweenNegative() {
		for(int i = 0; i < 1000; i++) {
			int temp = ((Number)Function.RANDBETWEEN.calculate(-5, -2)).intValue();
			/*if (temp < -5 || temp > -2) {
				System.out.println("i:=" + i + ", temp:=" + temp);
			}*/
			assertTrue(temp >= -4 && temp <= -1);
		}
	}
	@Test(expected = IllegalArgumentException.class)
	public void testRandBetweenError() {
		Function.RANDBETWEEN.calculate(5, 4);
	}
	@Test
	public void testRandBetweenSemiNegative() {
		for(int i = 0; i < 1000; i++) {
			int temp = ((Number)Function.RANDBETWEEN.calculate(-3, 5)).intValue();
			assertTrue(temp >= -3 && temp <= 5);
		}
	}
	@Test
	public void testRandBetweenDoubles() {
		for(int i = 0; i < 1000; i++) {
			int temp = ((Number)Function.RANDBETWEEN.calculate(0.6, 3.6)).intValue();
			assertTrue(temp >= 0 && temp <= 3);
		}
	}
	
	@Test
	public void testMedianOdd() {
		assertEquals(5, Function.MEDIAN.calculate(2, 5, -3, 6.5, 10));
	}
	@Test
	public void testMedianEven() {
		assertEquals(5.5, Function.MEDIAN.calculate(9.254, -32.2, 5, 6));
	}
	@Test
	public void testMedianRangeOdd() {
		assertEquals(8, Function.MEDIAN.calculate(r5));
	}
	
	@Test
	public void testMedianRangeEven() {
		assertEquals(9, Function.MEDIAN.calculate(r2));
	}

	@Test
	public void testIsNumberRangeInt() {
		assertEquals(true, Function.ISNUMBER.calculate(r2));
	}
	@Test
	public void testIsNumberRangeDouble() {
		assertEquals(true, Function.ISNUMBER.calculate(r6));
	}
	@Test
	public void testIsNumberRangeBool() {
		assertEquals(false, Function.ISNUMBER.calculate(r3));
	}
	@Test
	public void testIsNumberRangeString() {
		assertEquals(false, Function.ISNUMBER.calculate(r1));
	}
	
	@Test
	public void testIsEvenTrue() {
		assertEquals(true, Function.ISEVEN.calculate(6));
	}
	@Test
	public void testIsEvenFalse() {
		assertEquals(false, Function.ISEVEN.calculate(9));
	}
	@Test
	public void testIsEvenZero() {
		assertEquals(true, Function.ISEVEN.calculate(0));
	}
	
	@Test
	public void testIsOddTrue() {
		assertEquals(true, Function.ISODD.calculate(351));
	}
	@Test
	public void testIsOddFalse() {
		assertEquals(false, Function.ISODD.calculate(-500));
	}
	@Test
	public void testIsOddZero() {
		assertEquals(false, Function.ISODD.calculate(0));
	}
	
	@Test
	public void testAndy() {
		assertEquals("Hoi Andy!", Function.ANDY.calculate());
	}
	
	/*
	 * Test Exception behavior when no arguments are supplied
	 * Only on functions which need at least one argument.
	 */
	
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
	public void testRoundDownNoArgs() {
		Function.ROUNDDOWN.calculate();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRoundUpNoArgs() {
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
	public void testIsOddNoArgs() {
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
	
	@Test
	public void testGetArgumentList() {
		assertEquals("<b>value</b>, value, ...", Function.SUM.getArgumentList());
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

	@Test(expected=NumberFormatException.class)
	public void testDoubleValueOfString() {
		Function.doubleValueOf("This is a string");
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
	
	@Test(expected = NumberFormatException.class)
	public void testIntValueOfStringNoNum() {
		Function.intValueOf("hoi9");
	}
	@Test(expected = NumberFormatException.class)
	public void testIntValueOfStringNumInt() {
		Function.intValueOf("80hertz");
	}
	@Test
	public void testIntValueOfStringEmpty() {
		assertEquals(0, Function.intValueOf(""));
	}
	@Test
	public void testIntValueOfStringOnlyInt() {
		assertEquals(80, Function.intValueOf("80"));
	}
	@Test(expected = NumberFormatException.class)
	public void testIntValueOfStringOnlyDouble() {
		Function.intValueOf("80.9");
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
	public void testAssertMaxArgumentsError() {
		Function.assertMaxArguments(3, 4);
	}
	@Test
	public void testAssertMaxArgumentsOk1() {
		Function.assertMaxArguments(3, 3);
	}
	@Test
	public void testAssertMaxArgumentsOk2() {
		Function.assertMaxArguments(3, 0);
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