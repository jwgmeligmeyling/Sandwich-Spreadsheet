package Interfaces;

import File.Cell;

/**
 * Interface for Row class.
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Row {
	
	/**
	 * Get the Cell at a specific column index
	 * @param column index
	 * @return Cell at index
	 */
	public Cell getCell(int colIndex);
	
	/*
	 * Moet dit niet vanuit de andere kant bekeken worden?
	 * Wellicht een overbodige functie.
	 */
	public int getRowNum();
}