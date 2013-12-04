package File;

import java.util.ArrayList;

/**
 * 
 * @author Maarten Flikkema
 * @version 1.0
 * 
 * */
public class Sheet implements Interfaces.Sheet {

	private String sheetName;
	private ArrayList<Cell> cells;

	/**
	 * Constructor for declaring a new sheet
	 */
	public Sheet() {
		sheetName = "New sheet";
		cells = new ArrayList<Cell>();
	}

	/**
	 * Get the visible name of the Sheet.
	 * 
	 * @return sheetName
	 */
	public String getSheetName() {
		return sheetName;
	}

	/**
	 * Change the visible name of the Sheet.
	 * 
	 * @param newSheetName
	 *            will be the new name of the sheet (as visible for the user in
	 *            de tab)
	 */
	public void setSheetName(String newSheetName) {
		sheetName = newSheetName;
	}

	@Override
	public String getSheetNaam() {
		return sheetName;
	}

	@Override
	public ArrayList<Cell> getCells() {
		return cells;
	}

	@Override
	public Cell getCell(int rowIndex, int colIndex) {
		for (Cell temp : cells) {
			if (temp.getRow() == rowIndex && temp.getColumn() == colIndex) {
				return temp;
			}
		}
		return null;
	}

	@Override
	public ArrayList<Cell> getRowCells(int rowIndex) {
		ArrayList<Cell> tempList = new ArrayList<Cell>();

		for (Cell tempCell : cells) {
			if (tempCell.getRow() == rowIndex) {
				tempList.add(tempCell);
			}
		}
		return tempList;
	}

	@Override
	public ArrayList<Cell> getColumnCells(int colIndex) {
		ArrayList<Cell> tempList = new ArrayList<Cell>();
		
		for (Cell tempCell : cells) {
			if (tempCell.getColumn() == colIndex) {
				tempList.add(tempCell);
			}
		}
		return tempList;
	}
}