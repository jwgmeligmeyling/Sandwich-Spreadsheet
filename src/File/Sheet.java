package File;

import java.util.ArrayList;

/**
 * 
 * @author Maarten
 * @version 2013-11-25
 */
public class Sheet extends SpreadSheetFile {
	
	private String sheetName;
	private ArrayList<ArrayList<Cel>>[] cellen;
	
	/**
	 * Constructor for declaring a new sheet
	 */
	public Sheet() {
		sheetName = "New sheet";
	}
	
	/**
	 * Get the visible name of the Sheet.
	 * @return sheetName
	 */
	public String getSheetName() {
		return sheetName;
	}
	
	/**
	 * Change the visible name of the Sheet.
	 * @param newSheetName will be the new name of the sheet (as visible for the user in de tab)
	 */
	public void setSheetName(String newSheetName) {
		sheetName = newSheetName;
	}
}
