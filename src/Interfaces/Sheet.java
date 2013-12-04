package Interfaces;

import java.util.ArrayList;

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
	 * Method to get all cells
	 * @return ArrayListList of cells
	 */
	public ArrayList<File.Cell> getCells();

	/**
	 * Method to get the cell at given row and column index
	 * @param rowIndex is the index of the row the desired Cell is in
	 * @param colIndex is the index of the column the desired Cell is in
	 * @return row at given indexes
	 */
	public Cell getCell(int rowIndex, int colIndex);
	
	/**
	 * Method to get all cells in a given row
	 * @param rowIndex
	 * @return all the cells in the given row
	 */
	public ArrayList<File.Cell> getRowCells(int rowIndex);
	
	/**
	 * Method to get all cells in a given column
	 * @param colIndex
	 * @return all the cells in the given column
	 */
	public ArrayList<File.Cell> getColumnCells(int colIndex);
}