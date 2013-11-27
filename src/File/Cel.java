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
	private boolean hasFormula;
	
	/**
	 * Constructor voor Cel
	 * @param waardeIn
	 * @param typeIn
	 */
	public Cel(Object valueIn, String typeIn) {
		super();
		value = valueIn;
		type = typeIn;
		checkContent();
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
	 * @return true desda het Cel-object een formule bevat
	 * @see checkContent
	 */
	public boolean hasFormula() {
		return hasFormula;
	}

	/**
	 * Op basis van het eerste teken van de raw-waarde van Object waarde wordt
	 * bepaald of de inhoud als formule moet worden beschouwd (net als in Excel)
	 */
	public void checkContent() {
		hasFormula = (((String)value).substring(0, 1).equals("="));
	}
}
