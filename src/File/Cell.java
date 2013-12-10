package File;

import java.awt.Color;
import java.util.Observable;

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

	private CelType type;

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