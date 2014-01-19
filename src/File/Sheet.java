package File;

import java.util.HashMap;
import java.util.Map;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import GUI.STable;

/**
 * The <code>Sheet</code> class is the main class for the spreadsheet
 * 
 * @author Maarten Flikkema
 * @author Jan-Willem Gmelig Meyling
 */
public class Sheet implements Interfaces.Sheet, Cloneable {
	/*
	 * Class log: v1.0 Maarten Flikkema Sheet stub, with getters/setters and
	 * Interface implementation.
	 * 
	 * v1.1 Jan-Willem Gmelig Meyling Changed from ArrayLists to a HashMap with
	 * a Position class, and implementation of Range, which is now also used by
	 * the cell getters.
	 */

	/**
	 * The sheetName variable holds the name of the current sheet, for example:
	 * "New sheet...".
	 */
	private String sheetName;

	/**
	 * The cells <code>HashMap</code> binds <code>Position</code> instances to
	 * <code>Cell</code> instances. A <code>Position</code> holds a unique
	 * column and row index.
	 */
	final Map<Position, Cell> cells = new HashMap<Position, Cell>();

	/**
	 * The amount of columns used in this sheet. This is used to limit the
	 * amount of values returned by methods that return cell ranges. This value
	 * automatically increments when a <code>Cell</code> is inserted with a
	 * higher column index.
	 */
	private int columnCount = 0;

	/**
	 * The amount of rows used in this sheet. This is used to limit the amount
	 * of values returned by methods that return cell ranges. This value
	 * automatically increments when a <code>Cell</code> is inserted with a
	 * higher row index.
	 */
	
	private int rowCount = 0;
	private STable stable;

	/**
	 * Inner class for <code>Cell</code> positions. The <code>Position</code>
	 * class holds a unique column and row index. Uniqueness is ensured by
	 * overriding the <code>equals</code> and <code>hashCode</code> methods. <br>
	 * </br> Cells can also be looked up by creating a new <code>Position</code>
	 * object with the same parameters. For example, to get the first cell, you
	 * could use the following code:
	 * 
	 * <pre>
	 * return cells.get(new Position(0, 0));
	 * </pre>
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 */
	public class Position {
		/**
		 * Holds the column index for the current <code>Cell</code>.
		 */
		final int colIndex;

		/**
		 * Holds the row index for the current <code>Cell</code>.
		 */
		final int rowIndex;

		/**
		 * Constructor for the <code>Position</code> class. If a
		 * <code>Position</code> is initialized with a higher value for row
		 * index or column index than the current capacity, the capacity is
		 * updated.
		 * 
		 * @param colIndex
		 *            Index of the column
		 * @param rowIndex
		 *            Index of the row
		 * @throws AssertionError
		 *             When the indexes are negative values.
		 */
		public Position(int colIndex, int rowIndex) {
			assert colIndex >= 0 && rowIndex >= 0;

			this.colIndex = colIndex;
			this.rowIndex = rowIndex;

			if (colIndex > columnCount) {
				columnCount = colIndex;
			}

			if (rowIndex > rowCount) {
				rowCount = rowIndex;
			}
		}

		/**
		 * Method to get a new <code>Position</code> from this
		 * <code>Position</code> and the defined offset.
		 * 
		 * @param x
		 *            Column offset
		 * @param y
		 *            Row offset
		 * @return New <code>Position</code> instance
		 * @throws AssertionError
		 *             When the indexes with the applied offset are negative
		 *             values.
		 */
		public Position offset(int x, int y) {
			return new Position(colIndex + x, rowIndex + y);
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

		@Override
		public String toString() {
			return getColumnLetter(colIndex) + (rowIndex + 1);
		}
	}

	/**
	 * The range class is used to select a range of <code>Cell</code> instances
	 * from the current <code>Sheet</code>.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 * 
	 */
	public class Range implements Interfaces.Range {

		/**
		 * The start of the <code>Range</code>
		 */
		protected final Position topLeft;
		
		/**
		 * The end of the <code>Range</code>
		 */
		protected final Position bottomRight;
		
		/**
		 * The amount of columns in this <code>Range</code>
		 */
		private final int numColumns;
		
		/**
		 * The amount or rows in this <code>Range</code>
		 */
		private final int numRows;

		/**
		 * Constructor for a <code>Range</code>
		 * 
		 * @param colLeft
		 * @param rowUp
		 * @param colRight
		 * @param rowDown
		 * @throws AssertionError
		 *             When the indexes are negative values or the down right
		 *             values are not higher than the up left values.
		 */
		public Range(int colLeft, int rowUp, int colRight, int rowDown) {
			this(new Position(colLeft, rowUp), new Position(colRight, rowDown));
		}

		/**
		 * Constructor for a <code>Range</code>
		 * 
		 * @param topLeft
		 * @param bottomRight
		 * @throws AssertionError
		 *             When the indexes are negative values or the down right
		 *             values are not higher than the up left values.
		 */
		public Range(Position topLeft, Position bottomRight) {
			assert topLeft.colIndex <= bottomRight.colIndex;
			assert topLeft.rowIndex <= bottomRight.rowIndex;

			this.topLeft = topLeft;
			this.bottomRight = bottomRight;

			this.numColumns = bottomRight.colIndex - topLeft.colIndex + 1;
			this.numRows = bottomRight.rowIndex - topLeft.rowIndex + 1;
			
			createNullCells();
		}
		
		/**
		 * Some values for the {@code Cell} instances in this {@code Range}
		 * might be {@code null}, because they are not initialised. This may
		 * cause problems when calculating sums, or setting colors and text
		 * enhancements. Thus, now it's about time to create these missing
		 * {@code Cell} objects.
		 */
		private void createNullCells() {
			for ( int i = 0, l = numColumns * numRows; i < l; i++ ) {
				Position p = topLeft.offset(i / numRows, i % numRows);
				if ( cells.get(p) == null ) {
					new Cell(Sheet.this, p, "");
				}
			}
		}

		@Override
		public Cell[] getCellArray() {
			Cell[] output = new Cell[numColumns * numRows];
			for (int i = 0; i < output.length; i++) {
				output[i] = cells.get(topLeft.offset(i / numRows, i % numRows));
			}
			return output;
		}

		@Override
		public boolean contains(Cell cell) {
			return cell != null && cell.position != null
					&& cell.position.colIndex >= topLeft.colIndex
					&& cell.position.colIndex <= bottomRight.colIndex
					&& cell.position.rowIndex >= topLeft.rowIndex
					&& cell.position.rowIndex <= bottomRight.rowIndex;
		}

		@Override
		public boolean isSingleCell() {
			return topLeft.equals(bottomRight);
		}

		@Override
		public Cell firstCell() {
			return getCellAt(topLeft);
		}
		
		@Override
		public Position getTopLeft() {
			return topLeft;
		}

		@Override
		public int getRowCount() {
			return this.numRows;
		}

		@Override
		public int getColumnCount() {
			return this.numColumns;
		}

		@Override
		public String toString() {
			return topLeft.toString()
					+ ((isSingleCell()) ? "" : ":" + bottomRight.toString());
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof Range) {
				Range other = (Range) obj;
				return topLeft.equals(other.topLeft)
						&& bottomRight.equals(other.bottomRight);
			}
			return false;
		}
		
		@Override
		public Sheet getSheet() {
			return Sheet.this;
		}
	}

	/**
	 * A class for <code>Column</code> instances. Extends <code>Range</code>.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 */
	public class Column extends Range {

		/**
		 * The constructor for a <code>Column</code>. Takes a column index as
		 * argument, and creates a <code>Range</code> with all the
		 * <code>Cell</code> instances in at this column index.
		 * 
		 * @param colIndex
		 */
		public Column(int colIndex) {
			super(colIndex, 0, colIndex, rowCount);
		}
		
		@Override
		public String toString() {
			return topLeft.colIndex + ":" + topLeft.colIndex;
		}
	}

	/**
	 * A class for <code>row</code> instances. Extends <code>Range</code>.
	 * 
	 * @author Jan-Willem Gmelig Meyling
	 * 
	 */
	public class Row extends Range {
		
		/**
		 * The constructor for a <code>Row</code>. Takes a row index as
		 * argument, and creates a <code>Range</code> with all the
		 * <code>Cell</code> instances in at this row index.
		 * 
		 * @param rowIndex
		 */
		public Row(int rowIndex) {
			super(0, rowIndex, columnCount, rowIndex);
		}
		
		@Override
		public String toString() {
			return topLeft.rowIndex + ":" + topLeft.rowIndex;
		}
	}

	/**
	 * Constructor for declaring a new <code>Sheet</code>.
	 */
	public Sheet() {
		this("New sheet"); // TODO: misschien veranderen naar 'unnamed sheet'
							// o.i.d.??
	}

	/**
	 * Constructor for declaring a new <code>Sheet</code> with a name.
	 * 
	 * @param nameIn
	 */
	public Sheet(String nameIn) {
		sheetName = nameIn;
	}

	@Override
	public void init() {
		for (Cell cell : cells.values()) {
			cell.update();
		}
	}

	/**
	 * Method to get the letter ID for a column.
	 * @param index
	 * @return column letter
	 * Jan-Willem Gmelig Meyling
	 */
	public static String getColumnLetter(int index) {
		int quotient = (index) / 26;
		if (quotient > 0) {
			return getColumnLetter(quotient - 1) + (char) ((index % 26) + 65);
		} else {
			return "" + (char) ((index % 26) + 65);
		}
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
	public int getColumnCount() {
		return columnCount + 1;
	}

	@Override
	public void ensureColumnCount(int columnCount) {
		if (columnCount > this.columnCount) {
			this.columnCount = columnCount;
		}
	}

	@Override
	public int getRowCount() {
		return rowCount + 1;
	}

	@Override
	public void ensureRowCount(int rowCount) {
		if (rowCount > this.rowCount) {
			this.rowCount = rowCount;
		}
	}

	@Override
	public Cell createCell(String value, int colIndex, int rowIndex) {
		return  new Cell(this, new Position(colIndex, rowIndex), value);
	}

	@Override
	public Cell getCellAt(int colIndex, int rowIndex) {
		Position position = new Position(colIndex, rowIndex);
		Cell output = cells.get(position);
		if (output == null) {
			output = new Cell(this, position, "");
			cells.put(position, output);
		}
		return cells.get(new Position(colIndex, rowIndex));
	}
	
	@Override
	public Cell getCellAt(Position position) {
		return cells.get(position);
	}

	@Override
	public String getInputAt(int colIndex, int rowIndex) {
		Cell cell = cells.get(new Position(colIndex, rowIndex));
		if ( cell != null ) {
			return cell.getInput();
		}
		return null;
	}
	
	@Override
	public Object getValueAt(int colIndex, int rowIndex) {
		Cell cell = cells.get(new Position(colIndex, rowIndex));
		if ( cell != null ) {
			return cell.getValue();
		}
		return null;
	}
	
	@Override
	public void setValueAt(Object value, int colIndex, int rowIndex) {
		Position position = new Position(colIndex, rowIndex);
		Cell cell = cells.get(position);
		
		if ( cell == null ) {
			cell = new Cell(this, position, value.toString());
			cells.put(position, cell);
		} else {
			cell.setInput(value.toString());
		}
		
		cell.update();
	}

	@Override
	public Cell[] getCells() {
		return new Range(0, 0, columnCount, rowCount).getCellArray();
	}

	@Override
	public Row getRow(int rowIndex) {
		return new Row(rowIndex);
	}

	@Override
	public Column getColumn(int colIndex) {
		return new Column(colIndex);
	}

	@Override
	public Range getRange(Cell upLeft, Cell downRight) {
		return new Range(upLeft.position, downRight.position);
	}

	@Override
	public Range getRange(int colLeft, int rowUp, int colRight, int rowDown) {
		return new Range(new Position(colLeft, rowUp), new Position(colRight,
				rowDown));
	}

	@Override
	public void setSTable(STable stable) {
		this.stable = stable;
	}

	@Override
	public STable getSTable() {
		return stable;
	}

	/**
	 * The function that writes the sheet to a XML file. <div><b>Author:</b><br>
	 * <ul>
	 * <li>Jim Hommes</li>
	 * </ul>
	 * </div>
	 * 
	 * @param writer
	 * @throws XMLStreamException
	 *             If there was an error processing the XML stream
	 */
	@Override
	public void write(XMLStreamWriter writer) throws XMLStreamException {
		writer.writeStartElement("SPREADSHEET");
		writer.writeAttribute("name", sheetName);
		
		for (Cell cell : cells.values()) {
			cell.write(writer);
		}
		
		writer.writeEndElement();
	}

	/**
	 * XML Handler for SAX Parsing of Sheets
	 * 
	 * @author Jim Hommes
	 */
	public static class XMLHandler extends DefaultHandler {
		private final XMLReader reader;
		private final Workbook sheets;
		private final DefaultHandler fileHandler;

		/**
		 * Constructor for sheet parser
		 * @param sheets
		 * @param reader
		 * @param fileHandler
		 */
		public XMLHandler(Workbook sheets, XMLReader reader, DefaultHandler fileHandler) {
			this.reader = reader;
			this.sheets = sheets;
			this.fileHandler = fileHandler;
		}

		private Sheet sheet;

		@Override
		public void startElement(String uri, String localName, String name,
				Attributes attributes) throws SAXException {
			if (name.equalsIgnoreCase("SPREADSHEET")) {
				sheet = new Sheet();
				String sheetName = attributes.getValue("name");
				if (sheetName != null) {
					sheet.setSheetName(sheetName);
				}
			} else if (name.equalsIgnoreCase("CELL")) {
				DefaultHandler cellHandler = new Cell.XMLHandler(sheet, reader,
						this);
				reader.setContentHandler(cellHandler);
				cellHandler.startElement(uri, localName, name, attributes);
			}
		}

		@Override
		public void endElement(String uri, String localName, String name)
				throws SAXException {
			if (name.equalsIgnoreCase("SPREADSHEET")) {
				sheets.addSheet(sheet);
			} else {
				fileHandler.endElement(uri, localName, name);
				reader.setContentHandler(fileHandler);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		Sheet kloon = new Sheet(sheetName + " (Kopie)");
		for (Cell cell : cells.values()) {
			kloon.createCell(cell.getInput(), cell.position.colIndex,
					cell.position.rowIndex);
		}
		return kloon;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Sheet) {
			Sheet sheet = (Sheet) other;
			return sheet.cells.equals(cells);
		}
		return false;
	}

	@Override
	public String toString() {
		return cells.values().toString();
	}
}
