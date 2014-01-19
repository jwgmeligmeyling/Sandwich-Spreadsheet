package Interfaces;

import File.Cell;
import File.Sheet;
import File.Sheet.Position;

/**
 * The range class is used to select a range of <code>Cell</code> instances
 * from the current <code>Sheet</code>.
 * 
 * @author Jan-Willem Gmelig Meyling
 * @author Maarten Flikkema
 * 
 */
public interface Range {
	
	/**
	 * Method to get an array of <code>Cell</code> instances from this
	 * <code>Range</code>
	 * 
	 * @return array of <code>Cell</code> instances
	 */
	Cell[] getCellArray();
	
	/**
	 * Determine if the range contains a single <code>Cell</code>
	 * @return true if this <code>Cell</code> contains only one <code>Cell</code>
	 */
	boolean isSingleCell();
	
	/**
	 * @return the first <code>Cell</code> in this <code>Range</code>, or null if empty
	 */
	Cell firstCell();
	
	/**
	 * @return the number of rows in a Range
	 */
	int getRowCount();
	
	/**
	 * @return the number of columns in a Range
	 */
	int getColumnCount();

	/**
	 * @param cell {@code Cell}
	 * @return true if this {@code Range} contains the {@code Cell}
	 */
	boolean contains(Cell cell);

	/**
	 * @return the top left {@code Position} for this {@code Range}
	 */
	Position getTopLeft();

	/**
	 * @return the sheet for this Range
	 */
	Sheet getSheet();
}