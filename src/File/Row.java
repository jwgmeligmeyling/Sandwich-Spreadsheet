package File;

import java.util.ArrayList;

public class Row {
	
	private int rowNum;
	private ArrayList<Cel> columns;
	
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
		columns.add(celIn);
	}
	
	public ArrayList<Cel> getcolumns() {
		return columns;
	}
	
	/**
	 * 
	 * @param colIndex
	 * @return
	 */
	public Cel getCell(int colIndex) {
		return columns.get(colIndex);
	}
}