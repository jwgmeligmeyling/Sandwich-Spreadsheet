package File;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import File.Sheet.Range;

public class TestSheet {

	public Sheet sheet, sheet2, sheet3;
	public Cell A1,A2,A3,B1,B2,B3;
	public Range r1, r2, r3, r4;

	@Before
	public void setUp() throws Exception {
		sheet = new Sheet();
		sheet2 = new Sheet();
		sheet3 = new Sheet();
		A1 = sheet.createCell("bliep", 0, 0);
		A2 = sheet.createCell("5", 0, 1);
		A3 = sheet.createCell("=5", 0, 2);
		B1 = sheet.createCell("=5*2", 1, 0);
		B2 = sheet.createCell("=2+2*3", 1, 1);
		B3 = sheet.createCell("=ADD(5,3)", 1, 2);
		
		sheet2.createCell("bliep", 0, 0);
		sheet2.createCell("5", 0, 1);
		sheet2.createCell("=5", 0, 2);
		sheet2.createCell("=5*2", 1, 0);
		sheet2.createCell("=2+2*3", 1, 1);
		sheet2.createCell("=ADD(5,3)", 1, 2);
		
		r1 = sheet.getRange(A1, B3);
		r2 = sheet.getRange(A1, B3);
		r3 = sheet.getRange(A1, B2);
		r4 = sheet.getRange(B1, B3);
	}
	
	@Test
	public void sheetEquals() throws CloneNotSupportedException {
		Sheet clone = (Sheet) sheet.clone();
		assertEquals(clone, sheet);
		assertNotEquals(new Sheet(), sheet);
		assertNotEquals(new Sheet(), clone);
		assertNotEquals(null, sheet);
		assertNotEquals(new Object(), sheet);
	}
	
	@Test
	public void testGetSheetName() {
		assertEquals("New sheet", sheet.getSheetName());
	}

	@Test
	public void testSheetEqualsTrue() {
		assertTrue(sheet.equals(sheet2));
	}
	@Test
	public void testSheetEqualsFalse() {
		assertFalse(sheet.equals(sheet2));
		// TODO: WTF waarom assertation error hier?
	}
	@Test
	public void testSheetEqualsOtherType() {
		assertFalse(sheet.equals(r2));
	}
	
	@Test
	public void testEnsureColumnCount() {
		sheet.ensureColumnCount(100);
		assertEquals(101, sheet.getColumnCount());
		//TODO blijkbaar 101?????
	}
	
	@Test
	public void testEnsureColumnCount2() {
		sheet.ensureColumnCount(0);
		assertEquals(2, sheet.getColumnCount());
		// TODO: Waar komt die 51 vandaan?
	}

	@Test
	public void testEnsureRowCount() {
		sheet.ensureRowCount(1000);
		assertEquals(1001, sheet.getRowCount());
		//TODO blijkbaar 1001?????
	}
	
	@Test
	public void testEnsureRowCount2() {
		sheet.ensureRowCount(0);
		assertEquals(3, sheet.getRowCount());
	}
	
	@Test
	public void testSetSheetName() {
		sheet.setSheetName("testing sheetname");
		assertEquals("testing sheetname", sheet.getSheetName());
	}
	
	@Test
	public void testColumnCount() {
		// TODO waarom gaat dit nou fout??
		assertEquals(2, sheet.getColumnCount());
	}
	
	@Test 
	public void testRowCount() {
		assertEquals(3, sheet.getRowCount());
	}
	
	@Test
	public void rangeContains() {
		Range range = sheet.getRange(A2, B2);
		assertTrue(range.contains(B2));
		assertTrue(range.contains(A2));
		assertFalse(range.contains(A1));
		assertFalse(range.contains(B1));
		assertFalse(range.contains(B3));
		assertFalse(range.contains(A3));
	}
	
	@Test
	public void rangeToString() {
		assertEquals("A1:B2", sheet.getRange(A1,B2).toString());
		assertEquals("A1", sheet.getRange(A1, A1).toString());
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
	public void testGetCellAtNull() {
		assertNotEquals("bla", sheet.getCellAt(5, 5));
	}

	@Test
	public void testGetCellHigher() {
		assertEquals(B2, sheet.getCellAt(1, 1));
	}

	@Test
	public void testGetCells() {
		assertArrayEquals(new Cell[] { A1, A2, A3, B1, B2, B3 }, sheet.getCells());
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
	public void testRangeGetRowCount() {
		assertEquals(3, r1.getRowCount());
	}
	@Test
	public void testRangeGetColumnCount() {
		assertEquals(2, r1.getColumnCount());
	}

	@Test
	public void testGetColumnCells() {
		Cell[] temp = { B1, B2, B3 };
		assertArrayEquals(temp, sheet.getColumn(1).getCellArray());
	}

	@Test
	public void testGetRowCells() {
		Cell[] temp = { A2, B2 };
		assertArrayEquals(temp, sheet.getRow(1).getCellArray());
	}

	@Test
	public void testRangeEqualsOtherType() {
		assertFalse(r1.equals(A1));
	}
	@Test
	public void testRangeEquals() {
		assertEquals(true, r1.equals(r2));
	}
	@Test
	public void testRangeEquals2() {
		assertFalse(r1.equals(r3));
	}
	@Test
	public void testRangeEquals3() {
		assertFalse(r1.equals(r4));
	}

	
	@Test
	public void testGetColumnLetter() {
		assertEquals("A", Sheet.getColumnLetter(0));
	}
	@Test
	public void testGetColumnLetter2() {
		assertEquals("AB", Sheet.getColumnLetter(27));
	}
	@Test
	public void testGetColumnLetter3() {
		assertEquals("ZA", Sheet.getColumnLetter(676));
	}
	@Test
	public void testGetColumnLetter3_2() {
		assertEquals("AAP", Sheet.getColumnLetter(717));
	}
}