package Interfaces;

import java.awt.Color;

import File.CelType;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * Interface voor de klasse Cell.
 * @author Maarten Flikkema
 * @author Liam Clark
 * @author Jan-Willem Gmelig Meyling
 */
public interface Cell {
	
	/**
	 * Method to get the current value of the cell, for example "5", "4", "hello".
	 * @return value of this cell
	 */
	public Object getValue();
	
	/**
	 * Method to get the current input of the cell, for example "=ADD(3,2)", "4", "hello".
	 * @return input of this cell
	 */
	public String getInput();
	
	public boolean isChanged();

	/**
	 * Method to get the overlay data type of this cell.
	 * @see File.CelType
	 */
	public CelType getType();

	/**
	 * Method to get the foreground-color.
	 * @return foreground-color for this cell
	 */
	public Color getfColor();

	/**
	 * Method to get the background-color.
	 * @return backgroun-dcolor for this cell
	 */
	public Color getbColor();

	/**
	 * 
	 * @return true if the cell is formatted bold, else return false
	 */
	public boolean isBold();

	/**
	 * 
	 * @return true if the cell is formatted italic, else return false
	 */
	public boolean isItalic();

	/**
	 * 
	 * @return true if the cell is formatted underlined, else return false
	 */
	public boolean isUnderlined();

	/**
	 * Method to change the current input of the cell.
	 * @param newInput is the new input to parse
	 */
	public void setInput(String newInput);

	/**
	 * Method to get the overlay data type of this cell.
	 * @see File.CelType
	 * @param newType is the new data type
	 */
	public void setType(CelType typeIn);

	/**
	 * Set the foreground-color for this cell.
	 * @param newFColor is the new foreground color
	 */
	public void setfColor(Color fColorIn);

	/**
	 * Set the backgroud-color for this cell.
	 * @param newBColor is the new backgroud color
	 */
	public void setbColor(Color bColorIn);

	/**
	 * 
	 * @param boldIn
	 */
	public void setBold(boolean boldIn);

	/**
	 * 
	 * @param italicIn
	 */
	public void setItalic(boolean italicIn);

	/**
	 * 
	 * @param underlinedIn
	 */
	public void setUnderlined(boolean underlinedIn);
	
	/**
	 * @return the row index of the row (row 1 == index 0)
	 */
	public int getRow();
	
	/**
	 * @return the column index of the row (column A == index 0)
	 */
	public int getColumn();
	
	public Sheet getSheet();
	
	/**
	 * Check of de input van de cel een formule bevat.
	 * @return true als er een functie/formule in de input staat.
	 */
	public boolean isFunction();
	
	public String getPositionString();
	
	/**
	 * Method to write a cell to the XML-file
	 * @param writer XMLStreamWriter
	 * @throws XMLStreamException 
	 */
	public void write(XMLStreamWriter writer) throws XMLStreamException;
	
}