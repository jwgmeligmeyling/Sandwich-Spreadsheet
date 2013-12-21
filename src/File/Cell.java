package File;

import java.awt.Color;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import File.Sheet.Position;
import File.Sheet.Range;
import Parser.Parser;
import Parser.Function;

/**
 * Class for cells
 * @author Maarten Flikkema, Jan-Willem Gmelig Meyling
 */
public class Cell implements Comparable<Cell>, Interfaces.Cell {

	final Sheet sheet;
	final Position position;

	protected String input;
	protected Object value;
	protected boolean changed = false;
	
	private CelType type = CelType.TEXT;
	
	private Color fColor;
	private Color bColor;
	
	private boolean bold;
	private boolean italic;
	private boolean underlined;

	protected Vector<Cell> listeners = new Vector<Cell>();
	protected Vector<Cell> references = new Vector<Cell>();
	
	/**
	 * Constructor voor Cell
	 */
	public Cell(Sheet sheet, Position position, String input) {
		this.sheet = sheet;
		this.position = position;
		this.setInput(input);
	}
	
	/**
	 * Listen for changes in another cell
	 * @param other
	 */
	public void listen(Cell other) {
		if ( ! this.references.contains(other) ) {
			other.listeners.add(this);
			other.update(this);
			this.references.add(other);
		}
	}
	
	/**
	 * The listen method creates the references for all cells in a range
	 * @param range
	 */
	public void listen(Range range) {
		for ( Cell cell : range.getCellArray() ) {
			if ( cell != null ) { 
				listen(cell);
			}
		}
	}
	
	/**
	 * Clear all references, for instance when we set the input of this cell
	 */
	protected void clear() {
		for ( Cell reference : references ) {
			reference.listeners.remove(this);
		}
		references.clear();
	}
	
	/**
	 * Reparse this cell if changed, and recursively update listeners
	 */
	void update() {
		update((Cell) null);
	}
	
	/**
	 * Reparse this cell if changed, and recursively update listeners
	 */
	private void update(Cell cross) {
		if ( changed ) {
			value = Parser.parse(this);
			changed = false;
			
			for ( Cell listener : listeners ) {
				if ( listener == cross ) {
					continue;
				}
				
				listener.changed = true;
				listener.update(cross);
			}
		}
	}
	
	/**
	 * Reparse this cell if changed, and recursively update listeners
	 */
	public void update(AbstractTableModel tableModel) {
		if ( changed ) {
			value = Parser.parse(this);
			changed = false;
			tableModel.fireTableCellUpdated(getRow(), getColumn());
			
			for ( Cell listener : listeners ) {
				listener.changed = true;
				listener.update(tableModel);
			}
		}
	}

	@Override
	public String getInput() {
		return input;
	}

	@Override
	public Object getValue() {
		return value;
	}

	@Override
	public boolean isChanged() {
		return changed;
	}

	@Override
	public CelType getType() {
		return type;
	}

	@Override
	public Color getfColor() {
		return fColor;
	}

	@Override
	public Color getbColor() {
		return bColor;
	}

	@Override
	public boolean isBold() {
		return bold;
	}

	@Override
	public boolean isItalic() {
		return italic;
	}

	@Override
	public boolean isUnderlined() {
		return underlined;
	}

	@Override
	public void setInput(String input) {
		if ( ! input.equals(this.input) ) {
			this.clear();
			this.input = input;
			this.changed = true;
		}
	}

	@Override
	public void setType(CelType type) {
		this.type = type;
	}

	@Override
	public void setfColor(Color fColor) {
		this.fColor = fColor;
	}

	@Override
	public void setbColor(Color bColor) {
		this.bColor = bColor;
	}

	@Override
	public void setBold(boolean bold) {
		this.bold = bold;
	}

	@Override
	public void setItalic(boolean italic) {
		this.italic = italic;
	}

	@Override
	public void setUnderlined(boolean underlined) {
		this.underlined = underlined;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Cell) {
			Cell other = (Cell) obj;
			return position.equals(other.position) && ( value == null || value.equals(other.value));
		} else {
			return super.equals(obj);
		}
	}

	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public int compareTo(Cell o) {
		return (int) (Function.doubleValueOf(this) - Function.doubleValueOf(o));
	}

	@Override
	public Sheet getSheet() {
		return sheet;
	}

	@Override
	public boolean isFunction() {
		return !input.isEmpty() && input.charAt(0) == '=';
	}
	
	@Override
	public int getRow() {
		return position.rowIndex;
	}

	@Override
	public int getColumn() {
		return position.colIndex;
	}
	
	@Override
	public String getPositionString() {
		return position.toString();
	}
	
	@Override
	public void write(XMLStreamWriter writer) throws XMLStreamException {
		/*
		 * Start the <CELL> element and append the attributes
		 */
		writer.writeStartElement("CELL");
		writer.writeAttribute("row", Integer.toString(getRow()));
		writer.writeAttribute("column", Integer.toString(getColumn()));
		writer.writeAttribute("type", getType().toString());
		
		writer.writeCharacters(getInput());
		writer.writeEndElement();
	}

	/**
	 * XML Handler for SAX Parsing of Cells
	 * @author Jim Hommes
	 *
	 */
	public static class XMLHandler extends DefaultHandler {
		private final StringBuilder content;
	    private final Sheet sheet;
	    private int colIndex;
	    private int rowIndex;

		/**
		 * Constructor for Cell parser
		 * 
		 * @param sheet
		 *            The <code>sheet</code> to put this <code>Cell</code> in
		 */
		public XMLHandler(Sheet sheet) {
			this.sheet = sheet;
			this.content = new StringBuilder();
		}

		@Override
		public void characters(char[] ch, int start, int length)
				throws SAXException {
			/*
			 * characters can be called multiple times per element so aggregate
			 * the content in a StringBuilder
			 */
			content.append(ch, start, length);
		}

		@Override
		public void startElement(String uri, String localName, String name,
				Attributes attributes) throws SAXException {
			content.setLength(0);
			if (name.equalsIgnoreCase("CELL")) {
				colIndex = Integer.parseInt(attributes.getValue("column"));
				rowIndex = Integer.parseInt(attributes.getValue("row"));
			}
			// type?
		}

		@Override
		public void endElement(String uri, String localName, String name)
				throws SAXException {
			if (name.equalsIgnoreCase("CELL")) {
				sheet.createCell(content.toString(), colIndex, rowIndex);
			}
		}
	}
}