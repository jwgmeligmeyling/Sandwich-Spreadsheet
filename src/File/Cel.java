package File;

import java.awt.Color;

/**
 * 
 * @author Maarten Flikkema
 *
 */
public class Cel {
	/* 
	 * 
	 */
    private String formula;
	private String input;
	private String value;
	
	private Color bColor;
	private Color fColor;
	
	private String type;
	
	/**
	 * Constructor voor Cel
	 * @param waardeIn
	 * @param typeIn
	 */
	public Cel(String inputIn) {
		input = inputIn;
	}
	
	/**
	 * @return de raw-waarde van Object waarde
	 */
	public String getValue() {
		return value;
	}
	
	public String getInput() {
		return input;
	}

	/*
	 * public String getWaardeString() { return (String) waarde; } public int
	 * getWaardeInteger() { return (Integer) waarde; } public double
	 * getWaardeDouble() { return (Double) waarde; } public boolean
	 * getWaardeBoolenan() {return (Boolean) waarde; }
	 */
	
	public void setInput(String inputIn) {
		input = inputIn;
		updateValue();
	}
	
	public void updateValue() {
		
	}
	
	
	
	
	
	
	
	
	/**
	 * @return het opgeslagen type van de celinhoud of van het resultaat van de
	 *         formule in de cel
	 */
	public String getType() {
		return type;
	}
	
	/**
	 * @return true als het Cel-object een formule bevat
	 * @see checkContent
	 */
	public boolean hasFormula() {
		return (formula != null);
	}
}