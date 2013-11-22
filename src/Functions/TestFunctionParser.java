package Functions;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestFunctionParser {

	@Test
	public void testLiteralParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("5");
		assertEquals(5, f.toInteger());
	}
	
	@Test
	public void testLiteralParseFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("5");
		assertNotEquals(1, f.toInteger());
	}
	
	@Test
	public void testSingleAddParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(5,2)");
		assertEquals(7, f.toInteger());
	}	
	
	@Test
	public void testSingleAddParseFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(5,2)");
		assertNotEquals(5, f.toInteger());
	}

	@Test
	public void testMultiAddParseTrue() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(ADD(1,2),ADD(3,4))");
		assertEquals(10, f.toInteger());
	}
	
	@Test
	public void testMultiAddParseFalse() throws InstantiationException, IllegalAccessException {
		Function f = Function.parse("ADD(ADD(1,2),ADD(3,4))");
		assertNotEquals(5, f.toInteger());
	}

}
