package Parser;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestFunctions {

	@Test
	public void testAdd() {
		assertEquals(10, Function.ADD.calculate(5,5));
	}
	
	@Test
	public void testAddDoubles() {
		assertEquals(7.3, ((Double) Function.ADD.calculate(6.9, 0.4)).doubleValue(), 1e-15);
	}

	@Test
	public void testAddIntAndDouble() {
		assertEquals(7.25, Function.ADD.calculate(6, 1.25));
	}
	
	@Test
	public void testAddDoubleAndInt() {
		assertEquals(7.125, ((Double) Function.ADD.calculate(6.125, 1)).doubleValue(), 1e-15);
	}
	
}
