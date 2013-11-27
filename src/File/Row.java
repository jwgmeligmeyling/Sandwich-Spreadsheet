package File;

import java.util.ArrayList;

public class Row {
	
	private int rowNum;
	private ArrayList<Cel> cells;
	
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
	public void addCell(Cel celIn) {
		cells.add(celIn);
	}
	
	public ArrayList<Cel> getCells() {
		return cells;
	}
	
	/**
	 * 
	 * @param colIndex
	 * @return
	 */
	public Cel getCell(int colIndex) {
		return cells.get(colIndex);
	}
}