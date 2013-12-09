package Interfaces;

import java.util.ArrayList;

import File.Cell;
import File.Sheet;

/**
 * Interface for Range class.
 * @author Maarten Flikkema
 */
public interface Range {
	
	/**
	 * Method sets the name of a (user defined) range
	 * @param newName is the name the user wants to be able to use for the range
	 */
	public void setName(String newName);
	
	/**
	 * Method gets the name of an (user defined) range
	 * @return the name of the range
	 */
	public String getName();
	
	/**
	 * 
	 * @param upLeft is the first cell in the expected range
	 * @param downRight is the last cell in the expected range
	 * @return an ArrayList of cells with all the celsl in the range form Cell upLeft to Cell downRigth
	 */
	public void setCells(Sheet sheet, Cell upLeft, Cell downRight);
	
	/**
	 * 
	 * @param sheet
	 * @param rowUp is the row index of the upper left cell in the range
	 * @param colLeft is the column index of the upper left cell in the range
	 * @param rowDown is the row index of the last cell in the range
	 * @param colRight is the column index of the last cell in the range
	 * @return an ArrayList of cells with all the cells in the range from cell(rowUp, colLeft) to cell(rowDown, colRight)
	 */
	public void setCells(Sheet sheet, int rowUp, int colLeft, int rowDown, int colRight);
	
	/**
	 * 
	 * @return an ArrayList containing the Cells in the Range
	 */
	public ArrayList<Cell> getCells();
}