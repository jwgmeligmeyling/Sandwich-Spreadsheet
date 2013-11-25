public class Cel extends Sheet {

	protected Object waarde;
	protected String type;
	protected boolean hasFormula;

	public Cel(Object waardeIn, String typeIn) {
		waarde = waardeIn;
		type = typeIn;
		checkContent();
	}

	/**
	 * @return de raw-waarde van Object waarde
	 */
	public Object getWaarde() {
		return waarde;
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
		hasFormula = (((String) waarde).substring(0, 1).equals("="));
	}
}
