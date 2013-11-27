package Interfaces;

/**
 * @author Maarten Flikkema
 * @author Liam Clark
 */
import File.Cell;

public interface Row {

	public Cell getCell(int colIndex);
	
	public int getRowNum();
}
