package File;

import java.awt.Color;
import java.util.Observable;

/**
 * @author Maarten Flikkema
 */
public class Cell implements Interfaces.Cell {

	private String input;
	private String value;

	private CelType type;

	private Color fColor;
	private Color bColor;
	
	/**
	 * Constructor voor Cell
	 */
	public Cell(String inputIn, CelType typeIn, Color fColorIn, Color bColorIn) {
		input = inputIn;
		type = typeIn;
		fColor = fColorIn;
		bColor = bColorIn;
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
		
		
	}
}