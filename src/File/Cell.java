package File;

import java.awt.Color;
import java.util.Observable;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import File.Sheet.Position;
import Parser.Parser;

/**
 * @author Maarten Flikkema
 */
public class Cell extends Observable implements Interfaces.Cell {

	final Sheet sheet;
	final Position position;
	private String columnUser;

	private String input;
	private Object value;

	private CelType type = CelType.TEXT;

	private Color fColor;
	private Color bColor;

	/**
	 * Constructor voor Cell
	 */
	public Cell(Sheet sheet, Position position, String input) {
		this.sheet = sheet;
		this.position = position;
		this.input = input;
		updateValue();
	}
	
	/**
	 * Method to write a cell to the XML-file
	 * @param writer XMLStreamWriter
	 * @throws XMLStreamException 
	 */
	public void write(XMLStreamWriter writer) throws XMLStreamException {
		/*
		 * Start the <CELL> element and append the attributes
		 */
		writer.writeStartElement("CELL");
		writer.writeAttribute("row", Integer.toString(getRow()));
		writer.writeAttribute("column", Integer.toString(getColumn()));
		writer.writeAttribute("type", getType().toString());
		
		writer.writeCharacters(getInput());
		writer.writeEndElement();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Cell) {
			Cell other = (Cell) obj;
			return value.equals(other.value) && position.equals(other.position);
		} else {
			return super.equals(obj);
		}
	}

	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public void updateValue() {
		value = Parser.parse(sheet, input);
	}

	@Override
	public Object getValue() {
		return value;
	}

	@Override
	public String getInput() {
		return input;
	}

	@Override
	public void setInput(String newInput) {
		input = newInput;
		updateValue();
	}

	@Override
	public boolean inputIsFunction() {
		return !input.isEmpty() && input.charAt(0) == '=';
	}

	@Override
	public int getRow() {
		return position.rowIndex;
	}

	@Override
	public int getColumn() {
		return position.colIndex;
	}

	@Override
	public String getColumnString() {
		return columnUser;
	}

	@Override
	public CelType getType() {
		return type;
	}

	@Override
	public void setType(CelType newType) {
		type = newType;
	}

	@Override
	public Color getFColor() {
		return fColor;
	}

	@Override
	public void setFColor(Color newFColor) {
		fColor = newFColor;
	}

	@Override
	public Color getBColor() {
		return bColor;
	}

	@Override
	public void setBColor(Color newBColor) {
		bColor = newBColor;
	}

	@Override
	public void update(Observable o, Object arg) {
		// TODO Auto-generated method stub

	}
}