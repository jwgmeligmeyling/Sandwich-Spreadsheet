package Tests;

import static org.junit.Assert.*;

import java.awt.Color;

import org.junit.Before;
import org.junit.Test;

import File.Cell;
import File.Cell.CellType;
import File.Sheet;
import File.Sheet.Column;
import File.Sheet.Range;
import File.Sheet.Row;
import GUI.STable;

public class TestSheet {

	public Sheet sheet, sheet2, sheet3;
	public Cell A1,A2,A3,B1,B2,B3, C1, C2, C3;
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
		B3 = sheet.createCell("=SUM(5,3)", 1, 2);
		C1 = sheet.createCell("=SUM(B1:B3)", 2, 0);
		C2 = sheet.createCell("=COUNT(B1:B3)", 2, 1);
		C3 = sheet.createCell("=true", 2, 2);
		
		sheet.init();
		
		sheet2 = (Sheet) sheet.clone();
		sheet2.init();
		
		r1 = sheet.getRange(A1, B3);
		r2 = sheet.getRange(A1, B3);
		r3 = sheet.getRange(A1, B2);
		r4 = sheet.getRange(B1, B3);
	}
	
	@Test
	public void testCell() {
		assertTrue(B3.isFunction());
		assertFalse(A1.isFunction());
		assertEquals(A1, sheet2.getCellAt(0, 0));
		assertNotEquals(A1, null);
		assertNotEquals(A1, A2);
		
		A1.setbColor(Color.BLUE);
		assertEquals(A1.getbColor(), Color.BLUE);
		
		A1.setfColor(Color.RED);
		assertEquals(A1.getfColor(), Color.RED);
		
		assertFalse(A1.isItalic());
		A1.setItalic(true);
		assertTrue(A1.isItalic());
		

		assertFalse(A1.isBold());
		A1.setBold(true);
		assertTrue(A1.isBold());
		

		assertFalse(A1.isUnderlined());
		A1.setUnderlined(true);
		assertTrue(A1.isUnderlined());
		
		assertEquals(A1.getType(), CellType.TEXT);
		A1.setType(CellType.BOOLEAN);
		assertEquals(A1.getType(), CellType.BOOLEAN);
		
		assertEquals("A1 bliep", A1.toString());
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
		assertEquals(sheet, sheet2);
	}
	@Test
	public void testSheetEqualsFalse() {
		assertNotEquals(sheet, sheet3);
	}
	@Test
	public void testSheetEqualsOtherType() {
		assertNotEquals(sheet, r2);
	}
	
	@Test
	public void testEnsureColumnCount() {
		sheet.ensureColumnCount(100);
		assertEquals(101, sheet.getColumnCount());
	}
	
	@Test
	public void testEnsureColumnCount2() {
		sheet.ensureColumnCount(10);
		assertEquals(11, sheet.getColumnCount());
	}

	@Test
	public void testEnsureRowCount() {
		sheet.ensureRowCount(1000);
		assertEquals(1001, sheet.getRowCount());
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
		assertEquals(3, sheet.getColumnCount());
	}
	
	@Test 
	public void testRowCount() {
		assertEquals(3, sheet.getRowCount());
	}
	
	@Test
	public void testRangeEquals() {
		Range a = sheet.getRange(A1, B3);
		Range b = sheet.getRange(A1, B3);
		assertEquals(a,b);
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
		assertArrayEquals(new Cell[] { A1, A2, A3, B1, B2, B3, C1, C2, C3 },
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
	public void testRangeGetRowCount() {
		assertEquals(3, r1.getRowCount());
	}
	
	@Test
	public void testRangeTL() {
		assertEquals(r1.getTopLeft(), A1.getPosition());
	}
	
	@Test
	public void testgetValueAt() {
		assertEquals(sheet.getValueAt(0, 0), A1.getValue());
	}
	
	@Test
	public void testSetValueAt() {
		sheet.setValueAt("bliep1", 0, 0);
		assertEquals("bliep1", sheet.getValueAt(0, 0));
		assertEquals(sheet.getValueAt(0, 0), A1.getValue());
	}
	
	@Test
	public void testSTableConnection() {
		assertNull(sheet.getSTable());
		STable st = new STable(sheet, null);
		assertEquals(sheet.getSTable(), st);
	}
	
	@Test
	public void testContains() {
		assertTrue(r1.contains(A1));
		assertFalse(r1.contains(C3));
		sheet.toString();
	}
	
	@Test
	public void testRangeGetColumnCount() {
		assertEquals(2, r1.getColumnCount());
	}

	@Test
	public void testGetRowCells() {
		assertArrayEquals(new Cell[] { A2, B2, C2 }, sheet.getRow(1).getCellArray());
	}
	
	@Test
	public void testRangeEqualsOtherType() {
		assertNotEquals(r1, A2);
	}
	
	@Test
	public void testRangeEquals2() {
		assertNotEquals(r1, r3);
	}
	
	@Test
	public void testRangeEquals3() {
		assertNotEquals(r1, r4);
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
	
	@Test
	public void testGetInput() {
		assertEquals("=COUNT(B1:B3)", C2.getInput());
	}
	
	@Test
	public void testColumn() {
		Column column = sheet.new Column(0);
		assertEquals(sheet.new Range(A1.getPosition(), A3.getPosition()), column);
		assertEquals("A:A", column.toString());
	}
	
	@Test
	public void testRow() {
		Row row = sheet.new Row(0);
		assertEquals(sheet.new Range(A1.getPosition(), C1.getPosition()), row);
		assertEquals("1:1", row.toString());
	}
}