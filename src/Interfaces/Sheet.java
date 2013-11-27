package Interfaces;

import java.util.List;
import File.Cell;
import File.Row;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Sheet {
	
	/**
	 * Getter for the name of the current sheet
	 * @return name of the sheet
	 */
	public String getSheetNaam();
	
	/**
	 * Method to get all rows
	 * @return List of rows
	 */
	public List<Row> getRows();
	
	/**
	 * Method to get the row at supplied index.
	 * @param rowIndex is the index of the desired row (row number - 1)
	 * @return Row at the given row index
	 */
	public Row getRow(int rowIndex);

	/**
	 * Method to get the cell at given row and column index
	 * @param rowIndex is the index of the row the desired Cell is in
	 * @param colIndex is the index of the column the desired Cell is in
	 * @return row at given indexes
	 */
	public Cell getCell(int rowIndex, int colIndex);
}