package Interfaces;

import File.Cell;

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
	public Cell[] getRowCells(int rowIndex);
	
	/**
	 * Method to get all cells in a given column
	 * @param colIndex
	 * @return all the cells in the given column
	 */
	public Cell[] getColumnCells(int colIndex);
	
	/**
	 * 
	 * @param upLeft is the first cell in the expected range
	 * @param downRight is the last cell in the expected range
	 * @return an array of cells with all the celsl in the range form Cell upLeft to Cell downRigth
	 */
	public Cell[] getRangeCells(Cell upLeft, Cell downRight);
	
	/**
	 * 
	 * @param rowUpLeft is the row index of the upper left cell in the range
	 * @param colUpLeft is the column index of the upper left cell in the range
	 * @param rowDownRight is the row index of the last cell in the range
	 * @param colDownRight is the column index of the last cell in the range
	 * @return an array of cells with all the cells in the range from cell(rowUp, colLeft) to cell(rowDown, colRight)
	 */
	public Cell[] getRangeCells(int rowUp, int colLeft, int rowDown, int colRight);
}