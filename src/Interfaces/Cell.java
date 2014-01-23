package Interfaces;

import java.awt.Color;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import File.Cell.CellType;
import File.Sheet.Position;

/**
 * Interface voor de klasse Cell.
 * @author Maarten Flikkema
 * @author Liam Clark
 * @author Jan-Willem Gmelig Meyling
 */
public interface Cell {
	
	/**
	 * Listen for changes in a reference. A reference can be a single
	 * {@code Cell} or several cells in a {@code Range}. For every cell
	 * in a {@code Range}, this {@code Cell} subscribes as listener, and
	 * the other cell is appended to the references {@code Vector} for
	 * this {@code Cell}.
	 * @param other Another {@code Cell} instance, or {@code Range} containing
	 * cells.
	 */
	void listen(Object other);

	/**
	 * Method to get the current value of the cell, for example "5", "4", "hello".
	 * @return value of this cell
	 */
	Object getValue();
	
	/**
	 * Method to get the current input of the cell, for example "=ADD(3,2)", "4", "hello".
	 * @return input of this cell
	 */
	String getInput();
	
	boolean isChanged();

	/**
	 * Method to get the overlay data type of this cell.
	 */
	CellType getType();

	/**
	 * Method to get the foreground-color.
	 * @return foreground-color for this cell
	 */
	Color getfColor();

	/**
	 * Method to get the background-color.
	 * @return backgroun-dcolor for this cell
	 */
	Color getbColor();

	/**
	 * Method to het whether the visible text in a cell is formatted bold.
	 * @return true if the cell is formatted bold, else return false
	 */
	boolean isBold();

	/**
	 * Method to het whether the visible text in a cell is formatted italic.
	 * @return true if the cell is formatted italic, else return false
	 */
	boolean isItalic();

	/**
	 * Method to het whether the visible text in a cell is formatted underlined.
	 * @return true if the cell is formatted underlined, else return false
	 */
	boolean isUnderlined();

	/**
	 * Method to change the current input of the cell.
	 * @param newInput is the new input to parse
	 */
	void setInput(String newInput);

	/**
	 * Method to get the overlay data type of this cell.
	 * @param typeIn is the new data type
	 */
	void setType(CellType typeIn);

	/**
	 * Set the foreground-color for this cell.
	 * @param fColorIn is the new foreground color
	 */
	void setfColor(Color fColorIn);

	/**
	 * Set the backgroud-color for this cell.
	 * @param bColorIn is the new backgroud color
	 */
	void setbColor(Color bColorIn);

	/**
	 * Sets the bold property of the cell.
	 * @param boldIn
	 */
	void setBold(boolean boldIn);

	/**
	 * Sets the italic property of the cell.
	 * @param italicIn
	 */
	void setItalic(boolean italicIn);

	/**
	 * Sets the underlined property of the cell.
	 * @param underlinedIn
	 */
	void setUnderlined(boolean underlinedIn);
	
	/**
	 * @return the row index of the row (row 1 == index 0)
	 */
	int getRow();
	
	/**
	 * @return the column index of the row (column A == index 0)
	 */
	int getColumn();
	
	/**
	 * @return the sheet this Cell is in
	 */
	Sheet getSheet();
	
	/**
	 * Check if the input of the cel contains a function.
	 * @return true als er een functie/formule in de input staat.
	 */
	boolean isFunction();
	
	/**
	 * @return the position as a String
	 */
	String getPositionString();
	
	/**
	 * Method to write a cell to the XML-file
	 * @param writer is a XMLStreamWriter
	 * @throws XMLStreamException
	 */
	void write(XMLStreamWriter writer) throws XMLStreamException;

	/**
	 * @return the {@code Position} for this {@code Cell}
	 */
	Position getPosition();
}