package Interfaces;

import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import File.Cell;
import File.SpreadSheetFile;
import File.Sheet.Column;
import File.Sheet.Row;
import GUI.STable;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Sheet {
	
	/**
	 * Getter for the name of the current sheet
	 * @return name of the sheet
	 */
	public String getSheetName();
	
	/**
	 * Change the visible name of the Sheet.
	 * @param newSheetName will be the new name of the sheet (as visible for the user in de tab)
	 */
	public void setSheetName(String newSheetName);
	
	/**
	 * Method to get all cells
	 * @return Array of cells
	 */
	public Cell[] getCells();

	/**
	 * Method to get the cell at given row and column index
	 * @param rowIndex is the index of the row the desired Cell is in
	 * @param colIndex is the index of the column the desired Cell is in
	 * @return row at given indexes
	 */
	public Cell getCellAt(int colIndex, int rowIndex);
	
	/**
	 * Method to get all cells in a given row
	 * @param rowIndex
	 * @return all the cells in the given row
	 */
	public Row getRow(int rowIndex);
	
	/**
	 * Method to get all cells in a given column
	 * @param colIndex
	 * @return all the cells in the given column
	 */
	public Column getColumn(int colIndex);
	
	/**
	 * 
	 * @param upLeft is the first cell in the expected range
	 * @param downRight is the last cell in the expected range
	 * @return an array of cells with all the celsl in the range form Cell upLeft to Cell downRigth
	 */
	public Range getRange(Cell upLeft, Cell downRight);
	
	/**
	 * 
	 * @param rowUp is the row index of the upper left cell in the range
	 * @param colLeft is the column index of the upper left cell in the range
	 * @param rowDown is the row index of the last cell in the range
	 * @param colRight is the column index of the last cell in the range
	 * @return an array of cells with all the cells in the range from cell(rowUp, colLeft) to cell(rowDown, colRight)
	 */
	public Range getRange(int colLeft, int rowUp, int colRight, int rowDown);
	
	/**
	 * Update all cells in this sheet based on current input
	 */
	public void init();
	
	/**
	 * @return amount of columns in this {@code Sheet}
	 */
	public int getColumnCount();
	
	/**
	 *  Ensure column count, to extend this {@code Sheet}
	 * @param columnCount
	 */
	public void ensureColumnCount(int columnCount);

	/** 
	 * @return amount of rows in this {@code Sheet}
	 */
	public int getRowCount();
	
	/**
	 * Ensure row count, to extend this {@code Sheet}
	 * @param rowCount
	 */
	public void ensureRowCount(int rowCount);
	
	/**
	 * Alias for sheet.new Cell().
	 * @param value
	 * @param colIndex
	 * @param rowIndex
	 * @return newly created {@code Cell} object
	 */
	public Cell createCell(String value, int colIndex, int rowIndex);
	
	/**
	 * Setter for the STable instance referencing to this Sheet
	 * @param stable
	 */
	public void setSTable(STable stable);
	
	/**
	 * Getter for the STable instance referencing to this Sheet
	 * @return stable
	 */
	public STable getSTable();
}