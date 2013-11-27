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
	 * Method to get the row at supplied index
	 * @param index of the row
	 * @return Row at index
	 */
	public Row getRow(int rowIndex);

	/**
	 * Method to get the cell at given row and column index
	 * @param index of row
	 * @param index of column
	 * @return row at given indexes
	 */
	public Cell getCell(int rowIndex, int colIndex);
}