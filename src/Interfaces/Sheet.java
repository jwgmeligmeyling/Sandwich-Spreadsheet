package Interfaces;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import File.Cell;
import File.Sheet.Position;
import File.Sheet.Column;
import File.Sheet.Row;
import GUI.STable;

/**
 * The <code>Sheet</code> class is the main class for the spreadsheet
 * @author Jan-Willem Gmelig Meyling
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Sheet {
	
	/**
	 * Getter for the name of the current sheet
	 * @return name of the sheet
	 */
	String getSheetName();
	
	/**
	 * Change the visible name of the Sheet.
	 * @param newSheetName will be the new name of the sheet (as visible for the user in de tab)
	 */
	void setSheetName(String newSheetName);
	
	/**
	 * Method to get all cells
	 * @return Array of cells
	 */
	Cell[] getCells();

	/**
	 * Method to get the {@code Cell} at given row and column index
	 * @param rowIndex is the index of the row the desired Cell is in
	 * @param colIndex is the index of the column the desired Cell is in
	 * @return {@code Cell} at given {@code Position}
	 */
	Cell getCellAt(int colIndex, int rowIndex);
	
	/**
	 * Alias for {@link #getCellAt(int, int)}
	 * @param position
	 * @return {@code Cell} at given {@code Position}
	 */
	Cell getCellAt(Position position);

	/**
	 * Set the value for a {@code Cell} at a certain {@code Position}. If the
	 * {@code Cell} does not yet exists, a new {@code Cell} is created, such that
	 * unused cells in the table do not have to be stored in the file.
	 * @param value
	 * @param colIndex
	 * @param rowIndex
	 */
	void setValueAt(Object value, int colIndex, int rowIndex);

	/**
	 * @param colIndex
	 * @param rowIndex
	 * @return the calculated value of the {@code Cell} at the given {@code Position},
	 * 	or {@code null} if the {@code Cell} at that position is not filled.
	 */
	Object getValueAt(int colIndex, int rowIndex);

	/**
	 * @param colIndex
	 * @param rowIndex
	 * @return the input of the {@code Cell} at the given {@code Position}, or
	 * 	{@code null} if the {@code Cell} at that position is not filled.
	 */
	String getInputAt(int colIndex, int rowIndex);

	/**
	 * Method to get all cells in a given row
	 * @param rowIndex
	 * @return all the cells in the given row
	 */
	Row getRow(int rowIndex);
	
	/**
	 * Method to get all cells in a given column
	 * @param colIndex
	 * @return all the cells in the given column
	 */
	Column getColumn(int colIndex);
	
	/**
	 * 
	 * @param upLeft is the first cell in the expected range
	 * @param downRight is the last cell in the expected range
	 * @return an array of cells with all the celsl in the range form Cell upLeft to Cell downRigth
	 */
	Range getRange(Cell upLeft, Cell downRight);
	
	/**
	 * 
	 * @param rowUp is the row index of the upper left cell in the range
	 * @param colLeft is the column index of the upper left cell in the range
	 * @param rowDown is the row index of the last cell in the range
	 * @param colRight is the column index of the last cell in the range
	 * @return an array of cells with all the cells in the range from cell(rowUp, colLeft) to cell(rowDown, colRight)
	 */
	Range getRange(int colLeft, int rowUp, int colRight, int rowDown);
	
	/**
	 * Update all cells in this sheet based on current input
	 */
	void init();
	
	/**
	 * @return amount of columns in this {@code Sheet}
	 */
	int getColumnCount();
	
	/**
	 *  Ensure column count, to extend this {@code Sheet}
	 * @param columnCount
	 */
	void ensureColumnCount(int columnCount);

	/** 
	 * @return amount of rows in this {@code Sheet}
	 */
	int getRowCount();
	
	/**
	 * Ensure row count, to extend this {@code Sheet}
	 * @param rowCount
	 */
	void ensureRowCount(int rowCount);
	
	/**
	 * Alias for sheet.new Cell().
	 * @param value
	 * @param colIndex
	 * @param rowIndex
	 * @return newly created {@code Cell} object
	 */
	Cell createCell(String value, int colIndex, int rowIndex);
	
	/**
	 * Setter for the STable instance referencing to this Sheet
	 * @param stable
	 */
	void setSTable(STable stable);
	
	/**
	 * Getter for the STable instance referencing to this Sheet
	 * @return stable
	 */
	STable getSTable();

	/**
	 * The function that writes the sheet to a XML file.
	 * <div><b>Author:</b><br>
	 * <ul>
	 * <li>Jim Hommes</li>
	 * </ul>
	 * </div>
	 * 
	 * @param writer
	 * @throws XMLStreamException If there was an error processing the XML stream
	 */
	void write(XMLStreamWriter writer) throws XMLStreamException;
}
