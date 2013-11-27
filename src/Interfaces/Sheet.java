package Interfaces;

import java.util.ArrayList;
import File.Cell;
import File.Row;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 */
public interface Sheet {

	public String getSheetNaam();

	public Row getRow(int rowIndex);

	public ArrayList<Row> getRows();

	public Cell getCell(int rowIndex, int colIndex);

}
