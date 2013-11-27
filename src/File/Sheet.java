package File;

import java.util.ArrayList;

/**
 * 
 * @author Maarten
 * @version 2013-11-25
 */
public class Sheet implements Interfaces.Sheet {
	
	private String sheetName;
	private ArrayList<Row> rows;
	
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
	
	/**
	 * 
	 * @return ArrayList rows
	 */
	public ArrayList<Row> getRows() {
		return rows;
	}
	
	/**
	 * 
	 * @param rowIndex	rij-index
	 * @return de gevraagde rij
	 */
	public Row getRow(int rowIndex) {
		return rows.get(rowIndex);
	}
	
	public Cell getCell(int rowIndex, int colIndex) {
		return rows.get(rowIndex).getCell(colIndex);
	}
}