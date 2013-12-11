package Interfaces;

import File.Cell;

/**
 * Interface for Range class.
 * @author Maarten Flikkema
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
}