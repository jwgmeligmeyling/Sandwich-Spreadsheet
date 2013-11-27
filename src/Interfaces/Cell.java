package Interfaces;

import java.awt.Color;
import File.CelType;

/**
 * Interface voor de klasse Cell.
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Cell {
	
	public String getValue();
	public String getInput();
	public void setInput(String newInput);
	public void updateValue();
	
	public CelType getType();
	public void setType(CelType newType);
	
	public Color getFColor();
	public Color getBColor();
	public void setFColor(Color newFColor);
	public void setBColor(Color newBColor);
}