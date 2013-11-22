package Functions;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestFunctionParser {

	@Test
	public void testLiteralParseIntTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("5");
		assertEquals(5, f.toInteger());
	}
	
	@Test
	public void testLiteralParseIntFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("5");
		assertNotEquals(1, f.toInteger());
	}
	
	@Test
	public void testSingleAddParseIntTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(5,2)");
		assertEquals(7, f.toInteger());
	}	
	
	@Test
	public void testSingleAddParseIntFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(5,2)");
		assertNotEquals(5, f.toInteger());
	}

	@Test
	public void testMultiAddParseIntTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(ADD(1,2),ADD(3,4))");
		assertEquals(10, f.toInteger());
	}
	
	@Test
	public void testMultiAddParseIntFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(ADD(1,2),ADD(3,4))");
		assertNotEquals(5, f.toInteger());
	}
	
	@Test
	public void testMultiAddPowerIntParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(POWER(ADD(4,7),3),100)");
		assertEquals(1431, f.toInteger());
	}
	
	@Test
	public void testSingleSubtractIntParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("SUBTRACT(10,8)");
		assertEquals(2, f.toInteger());
	}
	
	@Test
	public void testMultiSubtractIntParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("SUBTRACT(20,SUBTRACT(180,175))");
		assertEquals(15, f.toInteger());
	}
	
	@Test
	public void testMultiAddSubtractIntParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(SUBTRACT(5,8),10)");
		assertEquals(7, f.toInteger());
	}
	
	@Test
	public void testSingleAddDoubleParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(5.9,3.4)");
		assertEquals(9.3, f.toDouble(), 1E-15);
	}
}