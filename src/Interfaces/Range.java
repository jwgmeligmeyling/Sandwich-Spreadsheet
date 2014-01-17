package Interfaces;

import File.Cell;

/**
 * Interface for Range class.
 * @author Maarten Flikkema
 * 
 */
public interface Range {
	
	/**
	 * Method sets the name of a (user defined) range
	 * @param newName is the name the user wants to be able to use for the range
	 */
//	public void setName(String newName);
	
	/**
	 * Method gets the name of an (user defined) range
	 * @return the name of the range
	 */
//	public String getName();
	
	/**
	 * Method to get an array of <code>Cell</code> instances from this
	 * <code>Range</code>
	 * 
	 * @return array of <code>Cell</code> instances
	 */
	public Cell[] getCellArray();
	
	/**
	 * Determine if the range contains a single <code>Cell</code>
	 * @return true if this <code>Cell</code> contains only one <code>Cell</code>
	 */
	public boolean isSingleCell();
	
	/**
	 * Return 
	 * @return the first <code>Cell</code> in this <code>Range</code>, or null if empty
	 */
	public Cell firstCell();
	
	/**
	 * @return the number of rows in a Range
	 */
	public int getRowCount();
	
	/**
	 * @return the number of columns in a Range
	 */
	public int getColumnCount();

	/**
	 * @param Another {@code Cell}
	 * @return true if this {@code Range} contains the {@code Cell}
	 */
	boolean contains(Cell cell);
}