package File;

import java.util.ArrayList;

/**
 * 
 * @author Maarten Flikkema
 * @version 1.0
 * 
 */
public class Sheet implements Interfaces.Sheet {
	
	private String sheetName;
	private ArrayList<Cell> cells;

	/**
	 * Constructor for declaring a new Sheet.
	 */
	public Sheet() {
		sheetName = "New sheet";
		cells = new ArrayList<Cell>();
	}
	
	public Sheet(String nameIn) {
		sheetName = nameIn;
		cells = new ArrayList<Cell>();
	}
	
	@Override
	public String getSheetName() {
		return sheetName;
	}
	
	@Override
	public void setSheetName(String newSheetName) {
		sheetName = newSheetName;
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
	
	@Override
	public ArrayList<Cell> getRangeCells(Cell upLeft, Cell downRight) {
		return getRangeCells(upLeft.getRow(), upLeft.getColumn(), downRight.getRow(), downRight.getColumn());
	}
	
	@Override
	public ArrayList<Cell> getRangeCells(int rowUp, int colLeft, int rowDown, int colRight) {
		ArrayList<Cell> tempList = new ArrayList<Cell>();
		
		for (Cell tempCell : cells) {
			if ((tempCell.getRow() >= rowUp && tempCell.getRow() <= rowDown) && (tempCell.getColumn() >= colLeft && tempCell.getColumn() <= colRight)) {
				tempList.add(tempCell);
				if (tempCell.getRow() > rowDown && tempCell.getColumn() > colRight) { break; }
			}
		}
		return tempList;
	}
}