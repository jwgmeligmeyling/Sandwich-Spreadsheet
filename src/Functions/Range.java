package Functions;

import java.util.ArrayList;
import File.Cell;
import File.Sheet;

/*
 * Verzoek om deze klasse te verplaatsen naar package "File" - Maarten
 */

/**
 * This class can be used to define parts of sheets. One purpose for this is the
 * so called user defined range, which makes it posible for users to refer to
 * ranges with a name in stead of via cell references.
 * 
 * @author Maarten Flikkema
 * 
 */
public class Range implements Interfaces.Range {

	private ArrayList<Cell> cells;
	private String name;

	public Range(Sheet sheet, Cell upLeft, Cell downRight) {
		cells = sheet.getRangeCells(upLeft, downRight);
	}

	public Range(Sheet sheet, int rowUp, int colLeft, int rowDown, int colRight) {
		cells = sheet.getRangeCells(rowUp, colLeft, rowDown, colRight);
	}

	public void setName(String newName) {
		name = newName;
	}

	public String getName() {
		return name;
	}

	@Override
	public void setCells(Sheet sheet, Cell upLeft, Cell downRight) {
		cells = sheet.getRangeCells(upLeft, downRight);
	}

	@Override
	public void setCells(Sheet sheet, int rowUp, int colLeft, int rowDown, int colRight) {
		cells = sheet.getRangeCells(rowUp, colLeft, rowDown, colRight);
	}

	@Override
	public ArrayList<Cell> getCells() {
		return cells;
	}
}