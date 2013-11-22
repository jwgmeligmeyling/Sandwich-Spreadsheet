public class Cel {
	
	protected Object waarde;
	protected String type;
	
	public Cel(Object waardeIn, String typeIn) {
		waarde = waardeIn;
		type = typeIn;
	}
	
	public Object getWaarde() { return waarde; }
	/*
	public String getWaardeString() {	return (String) waarde; }
	public int getWaardeInteger() {		return (Integer) waarde; }
	public double getWaardeDouble() {	return (Double) waarde; }
	public boolean getWaardeBoolenan() {return (Boolean) waarde; }
	*/
	
	public String getType() {
		return type;
		/*
		if (waarde instanceof String) {
			return "String";
		} else if (waarde instanceof Double) {
			return "Double";
		} else if (waarde instanceof Integer) {
			return "Integer";
		} else if (waarde instanceof Boolean) {
			return "Boolean";
		} else {
			return null;
		}
		*/
	}

}
