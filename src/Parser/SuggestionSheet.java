package Parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SuggestionSheet {
	int rowCount = 0;
	int columnCount = 0;
	
	private class Position {
		private final int colIndex;
		private final int rowIndex;
		
		Position(int colIndex, int rowIndex) {
			this.colIndex = colIndex;
			this.rowIndex = rowIndex;
			
			if ( colIndex > columnCount ) {
				columnCount = colIndex;
			}
			
			if ( rowIndex > rowCount ) {
				columnCount = colIndex;
			}
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + colIndex;
			result = prime * result + rowIndex;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Position other = (Position) obj;
			if (colIndex != other.colIndex)
				return false;
			if (rowIndex != other.rowIndex)
				return false;
			return true;
		}	
	}
	
	private static class Cell {
		private String function;
		private String value;
		
		public Cell(String function) {
			this.function = function;
		}
		
		public String getFunction() {
			return function;
		}
		
		public void setFunction(String function) {
			this.function = function;
		}
		
		public String getValue() {
			return value;
		}
		
		public void setValue(String value) {
			this.value = value;
		}
	}

	private Map<Position, Cell> cells = new HashMap<Position, Cell>();

	public SuggestionSheet() {
		// TODO Auto-generated constructor stub
	}
	
	private void setCellAt(Cell cell, int colIndex, int rowIndex) {
		cells.put(new Position(colIndex, rowIndex), cell);
	}
	
	private Cell getCellAt(int colIndex, int rowIndex) {
		return cells.get(new Position(colIndex, rowIndex));
	}
	
	private List<Cell> getColumn(int colIndex) {
		List<Cell> result = new ArrayList<Cell>();
		for ( int rowIndex = 0; rowIndex < rowCount; rowIndex++ ) {
			result.add(cells.get(new Position(colIndex, rowIndex)));
		}
		return result;
	}
	
	private List<Cell> getRow(int rowIndex) {
		List<Cell> result = new ArrayList<Cell>();
		for ( int colIndex = 0; colIndex < columnCount; colIndex++ ) {
			result.add(cells.get(new Position(colIndex, rowIndex)));
		}
		return result;
	}
	
	private List<List<Cell>> getRange(int colIndexTopLeft, int rowIndexTopLeft,
			int colIndexBottomRight, int rowIndexBottomRight) {
		assert colIndexTopLeft <= colIndexBottomRight;
		assert rowIndexTopLeft <= rowIndexBottomRight;

		List<List<Cell>> columns = new ArrayList<List<Cell>>();
		
		for ( int colIndex = colIndexTopLeft; colIndex < colIndexBottomRight; colIndex++) {
			List<Cell> column = new ArrayList<Cell>();
			for ( int rowIndex = rowIndexTopLeft; rowIndex < rowIndexBottomRight; rowIndex++ ) {
				column.add(cells.get(new Position(colIndex, rowIndex)));
			}
			columns.add(column);
		}
		
		return columns;
	}

}
