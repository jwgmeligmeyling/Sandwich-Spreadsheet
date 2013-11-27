package Interfaces;

import java.awt.Color;
import java.util.Observer;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 * @author Jan-Willem Gmelig Meyling
 */
public interface Cell extends Observer {
	
	/**
	 * Method to get the current value of the cell, for example "5"
	 * @return value of this cell
	 */
	public String getValue();
	
	/**
	 * Method to get the current input of the cell, for example "ADD(3,2)"
	 * @return input of this cell
	 */
	public String getInput();
	
	/**
	 * Method to change the current input of the cell
	 * @param input to parse
	 */
	public void setInput(String newInput);

	/**
	 * Method to get the foreground-color
	 * @return foreground-color for this cell
	 */
	public Color getFColor();
	
	/**
	 * Method to get the background-color
	 * @return backgroun-dcolor for this cell
	 */
	public Color getBColor();

	/**
	 * Set the foreground-color for this cell
	 * @param foreground-color
	 */
	public void setFColor(Color newFColor);
	
	/**
	 * Set the backgroud-color for this cell
	 * @param backgroud-color
	 */
	public void setBColor(Color newBColor);
}