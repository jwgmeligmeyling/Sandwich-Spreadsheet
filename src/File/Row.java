package File;

import java.util.ArrayList;

public class Row implements Interfaces.Row {
	
	private int rowNum;
	private ArrayList<Cell> cells;
	
	/**
	 * Constructor voor Row
	 * @param rowNumIn
	 */
	public Row(int rowNumIn) {
		rowNum = rowNumIn;
	}
	
	/**
	 * 
	 * @param celIn
	 */
	public void addCell(Cell celIn) {
		cells.add(celIn);
	}
	
	
	public ArrayList<Cell> getcolumns() {
		return cells;
	}
	
	/**
	 * Get the Cell at a given column index in a Row (this)
	 * @param colIndex is the column index the desired Cell is in
	 * @return Cell at the given column index in this Row
	 */
	@Override
	public Cell getCell(int colIndex) {
		return cells.get(colIndex);
	}

	@Override
	public int getRowNum() {
		return rowNum;
	}
}