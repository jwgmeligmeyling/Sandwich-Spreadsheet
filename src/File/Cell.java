package File;

import java.awt.Color;
import java.util.Observable;

/**
 * @author Maarten Flikkema
 */
public class Cell extends Observable implements Interfaces.Cell {

	private int row;
	private int column;
	private String columnUser;
	
	private String input;
	private String value;
	
	private CelType type;
	
	private Color fColor;
	private Color bColor;
	
	
	/**
	 * Constructor voor Cell
	 */
	public Cell(String inputIn) {
		input = inputIn;
		updateValue();
	}
	
	@Override
	public void updateValue() {
		/*
		 * Hier moet de value van de Cell opnieuw worden berekend namens input.
		 */
	}
	
	@Override
	public String getValue() { return value; }
	
	@Override
	public String getInput() { return input; }
	@Override
	public void setInput(String newInput) {
		input = newInput;
		updateValue();
	}
	
	@Override
	public boolean inputIsFunction() {
		if (!input.equals("")) {
			return (input.substring(0, 1).equals("="));
		} else {
			return false;
		}
	}
	
	@Override
	public int getRow() {return row; }
	@Override
	public int getColumn() { return column; }
	@Override
	public String getColumnString() { return columnUser; }
	
	@Override
	public CelType getType() { return type; }
	@Override
	public void setType(CelType newType) { type = newType; }
	
	@Override
	public Color getFColor() { return fColor; }
	@Override
	public void setFColor(Color newFColor) { fColor = newFColor; }
	
	@Override
	public Color getBColor() { return bColor; }
	@Override
	public void setBColor(Color newBColor) { bColor = newBColor; }
	
	@Override
	public void update(Observable o, Object arg) {
		// TODO Auto-generated method stub
		
	}
}