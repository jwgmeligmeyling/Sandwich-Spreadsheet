package File;

/**
 * 
 * @author Maarten Flikkema
 *
 */
public class Cel {
	/* 
	 * 
	 */
	private int column;
    private String formula;
	private Object value;
	private String type;
	
	/**
	 * Constructor voor Cel
	 * @param waardeIn
	 * @param typeIn
	 */
	public Cel(int column, Object valueIn, String typeIn) {
		super();
		value = valueIn;
		type = typeIn;
	}
	
	/**
	 * @return de raw-waarde van Object waarde
	 */
	public Object getValue() {
		return value;
	}

	/*
	 * public String getWaardeString() { return (String) waarde; } public int
	 * getWaardeInteger() { return (Integer) waarde; } public double
	 * getWaardeDouble() { return (Double) waarde; } public boolean
	 * getWaardeBoolenan() {return (Boolean) waarde; }
	 */
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