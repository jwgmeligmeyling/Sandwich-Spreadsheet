package Interfaces;

import java.awt.Color;

import File.CelType;

import java.util.Observer;

/**
 * Interface voor de klasse Cell.
 * @author Maarten Flikkema
 * @author Liam Clark
 * @author Jan-Willem Gmelig Meyling
 */
public interface Cell extends Observer {
	
	/**
	 * Method to get the current value of the cell, for example "5", "4", "hello".
	 * @return value of this cell
	 */
	public String getValue();
	
	/**
	 * Method to get the current input of the cell, for example "=ADD(3,2)", "4", "hello".
	 * @return input of this cell
	 */
	public String getInput();
	
	/**
	 * Check of de input van de cel een formule bevat.
	 * @return true als er een functie/formule in de input staat.
	 */
	public boolean inputIsFunction();	
	
	/**
	 * Method to change the current input of the cell.
	 * @param newInput is the new input to parse
	 */
	public void setInput(String newInput);
	
	/**
	 * Method to update the value parameter.
	 * Checks is input contains a formula, if it does, it calculates the formula, else is takes over the value of input
	 */
	public void updateValue();
	
	
	/**
	 * @return the row index of the row (row 1 == index 0)
	 */
	public int getRow();
	
	/**
	 * @return the column index of the row (column A == index 0)
	 */
	public int getColumn();
	
	/**
	 * @return the column in user friendly form (e.a.: "A", "F", "AF") (column A == column 1 == index 0)
	 */
	public String getColumnString();
	
	/**
	 * Method to get the overlay data type of this cell.
	 * @see File.CelType
	 */
	public CelType getType();
	
	/**
	 * Method to get the overlay data type of this cell.
	 * @see File.CelType
	 * @param newType is the new data type
	 */
	public void setType(CelType newType);
	
	/**
	 * Method to get the foreground-color.
	 * @return foreground-color for this cell
	 */
	public Color getFColor();
	
	/**
	 * Method to get the background-color.
	 * @return backgroun-dcolor for this cell
	 */
	public Color getBColor();
	
	/**
	 * Set the foreground-color for this cell.
	 * @param newFColor is the new foreground color
	 */
	public void setFColor(Color newFColor);
	
	/**
	 * Set the backgroud-color for this cell.
	 * @param newBColor is the new backgroud color
	 */
	public void setBColor(Color newBColor);
}