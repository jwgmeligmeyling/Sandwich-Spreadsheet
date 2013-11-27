package Interfaces;

import File.Cell;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Row {
	
	/**
	 * Get the row at a specific column index
	 * @param column index
	 * @return row at index
	 */
	public Cell getCell(int colIndex);
	
	/*
	 * Moet dit niet vanuit de andere kant bekeken worden?
	 * Wellicht een overbodige functie.
	 */
	public int getRowNum();
}
