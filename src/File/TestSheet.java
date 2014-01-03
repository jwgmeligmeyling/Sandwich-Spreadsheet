package File;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;

public class TestSheet {

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
	public void testGetCellAt() {
		assertEquals(A1, sheet.getCellAt(0, 0));
	}

	@Test
	public void testGetCellAtFalse() {
		assertNotEquals(A2, sheet.getCellAt(0, 0));
	}

	@Test
	public void testGetCellHigher() {
		assertEquals(B2, sheet.getCellAt(1, 1));
	}

	@Test
	public void testGetCells() {
		assertArrayEquals(new Cell[] { A1, A2, A3, B1, B2, B3 },
				sheet.getCells());
	}

	@Test
	public void testGetRangeSingle() {
		assertArrayEquals(new Cell[] { A1 }, sheet.getRange(0, 0, 0, 0).getCellArray());
	}

	@Test
	public void testGetRangeSingleByCell() {
		assertArrayEquals(new Cell[] { A1 }, sheet.getRange(A1, A1).getCellArray());
	}

	@Test
	public void testGetRangeMulti() {
		assertArrayEquals(new Cell[] { A2, A3, B2, B3 }, sheet.getRange(0, 1, 1, 2).getCellArray());
	}

	@Test
	public void testGetRangeMultiByCell() {
		assertArrayEquals(new Cell[] { A2, A3, B2, B3 }, sheet.getRange(A2, B3).getCellArray());
	}

	@Test
	public void testGetColumnCells() {
		assertArrayEquals(new Cell[] { B1, B2, B3 }, sheet.getColumn(1).getCellArray());
	}

	@Test
	public void testGetRowCells() {
		assertArrayEquals(new Cell[] { A2, B2 }, sheet.getRow(1).getCellArray());
	}
}