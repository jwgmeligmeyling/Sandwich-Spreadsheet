package Interfaces;

/**
 * Interface for <code>Cell</code> positions. The <code>Position</code>
 * class holds a unique column and row index.
 * @author Jan-Willem Gmelig Meyling
 */
public interface Position {
	
	/**
	 * @return the row index of row index of the position
	 */
	int getRow();
	
	/**
	 * @return the column index of the row
	 */
	int getColumn();
	
	/**
	 * Method to get a new {@code Position} from this {@code Position} and the defined offset.
	 * @param x Column offset
	 * @param y Row offset
	 * @return New <code>Position</code> instance
	 * @throws AssertionError When the indexes with the applied offset are negative values
	 */
	Position offset(int x, int y);
}