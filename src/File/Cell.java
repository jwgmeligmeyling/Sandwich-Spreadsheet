package File;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import File.Sheet.Position;
import File.Sheet.Range;
import Parser.Parser;

/**
 * Class for cells
 * @author Maarten Flikkema, Jan-Willem Gmelig Meyling
 */
public class Cell implements Interfaces.Cell {

	final Sheet sheet;
	final Position position;

	protected String input;
	protected Object value;
	protected boolean changed = false;
	
	private CellType type = CellType.TEXT;
	
	private Color fColor;
	private Color bColor;
	
	private boolean bold;
	private boolean italic;
	private boolean underlined;

	protected Vector<Cell> listeners = new Vector<Cell>();
	protected Vector<Cell> references = new Vector<Cell>();
	
	/**
	 * Constructor for a empty cell
	 * @param sheet
	 * @param position
	 */
	public Cell(Sheet sheet, Position position) {
		this(sheet, position, "");
	}
	
	/**
	 * Constructor for a cell
	 * @param sheet
	 * @param position
	 * @param input
	 */
	public Cell(Sheet sheet, Position position, String input) {
		this.sheet = sheet;
		this.position = position;
		this.setInput(input);
		sheet.cells.put(position,  this);
	}
	
	@Override
	public void listen(Object other) {
		if ( other == null ) {
			return;
		} else if ( other instanceof Cell ) {
			Cell cell = (Cell) other;
			if ( ! this.references.contains(cell) ) {
				cell.listeners.add(this);
				cell.update(this);
				this.references.add(cell);
			}
		} else if ( other instanceof Range ) {
			Range range = (Range) other;
			for ( Cell cell : range.getCellArray() ) {
				listen(cell);
			}
		}
	}
	
	/**
	 * Reparse this cell if changed, and recursively update listeners. This method
	 * is called from the {@code Sheet}'s Initialize function, which requires all
	 * Cells to be calculated in the right order.
	 */
	void update() {
		update((Cell) null);
	}
	
	/**
	 * Reparse this cell if changed, and recursively update listeners
	 */
	private void update(Cell cross) {
		// Is the value changed, or did the user just open the editor?
		if ( changed ) {
			try {
				// Parse the value
				value = Parser.parse(this);
				checkConflicts();
			} catch ( Exception e ) {
				value = "#VALUE";
				this.clear();
				if ( e != null && sheet != null ) 
					sheet.onException(e);
			} finally {
				changed = false;
				// Update the listeners recursively
				for ( Cell listener : listeners ) {
					if ( listener == cross ) {
						continue;
					}
					
					listener.changed = true;
					listener.update(cross);
				}
			}
		}
	}
	
	private void checkConflicts() {
		List<Object> rlisteners = new ArrayList<Object>(references);
		
		for ( int i = 0; i < rlisteners.size(); i++ ) {
			Object listener = rlisteners.get(i);
			if ( listener.equals(this) ) {
				throw new IllegalArgumentException("Crossreference for cell " + position.toString());
			} else if ( listener instanceof Cell ) {
				for ( Object sublistener : ((Cell) listener).references ) {
					if (! rlisteners.contains(sublistener)) {
						rlisteners.add(sublistener);
					}
				}
			} else if ( listener instanceof Range ) {
				for ( Object rangecell : ((Range) listener).getCellArray() ) {
					if ( ! rlisteners.contains(rangecell) ) {
						rlisteners.add(rangecell);
					}
				}
			}
		}
	}

	/**
	 * Clear all references, for instance when we set the input of this cell
	 */
	private void clear() {
		for ( Cell reference : references ) {
			reference.listeners.remove(this);
		}
		references.clear();
	}

	@Override
	public String getInput() {
		if ( input != null ) {
			return input;
		}
		return "";
	}

	@Override
	public Object getValue() {
		if (value != null) {
			return value;
		}
		return getInput();
	}

	@Override
	public boolean isChanged() {
		return changed;
	}

	@Override
	public CellType getType() {
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
		this.clear();
		this.input = input;
		this.changed = true;
	}

	@Override
	public void setType(CellType type) {
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
	public Position getPosition() {
		return position;
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
	public boolean equals(Object obj) {
		if (obj instanceof Cell) {
			Cell other = (Cell) obj;
			return position.equals(other.position) && (input == null || input.equals(other.input));
		} else {
			return super.equals(obj);
		}
	}

	@Override
	public String toString() {
		return position.toString() + " " + (( value == null ) ? input : value.toString());
	}

	/**
	 * Enum for the various CellType a Cell can have
	 * @author Jan-Willem Gmelig Meyling
	 * @author Maarten Flikkema
	 *
	 */
	public static enum CellType {
		/**
		 * Try to convert the result to a Number string
		 */
		NUMBER,
		
		/**
		 * Try to convert the result to a Boolean String
		 */
		BOOLEAN,
		
		/**
		 * Convert the result to a regular String
		 */
		TEXT,
		
		/**
		 * Show the value as Date
		 */
		DATE;
		
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
		
		if ( fColor != null ) 
			writer.writeAttribute("fcolor", "" + fColor.getRGB());
		if ( bColor != null ) 
			writer.writeAttribute("bcolor", "" + bColor.getRGB());
		if ( underlined )
			writer.writeAttribute("underlined", "true");
		if ( bold )
			writer.writeAttribute("bold", "true");
		if ( italic ) 
			writer.writeAttribute("italic", "true");
		
		writer.writeCharacters(getInput());
		writer.writeEndElement();
	}

	/**
	 * XML Handler for SAX Parsing of Cells
	 * @author Jim Hommes
	 *
	 */
	public static class XMLHandler extends DefaultHandler {
		
		private final StringBuilder content = new StringBuilder();
	    private final XMLReader reader;
	    private final DefaultHandler sheetHandler;
	    private final Sheet sheet;
	    
	    private Cell cell;
	    
		/**
		 * Constructor for Cell parser
		 * 
		 * @param sheet
		 *            The <code>sheet</code> to put this <code>Cell</code> in
		 */
		public XMLHandler(Sheet sheet, XMLReader reader, DefaultHandler sheetHandler) {
			this.sheet = sheet;
			this.reader = reader;
			this.sheetHandler = sheetHandler;
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
				int colIndex = Integer.parseInt(attributes.getValue("column"));
				int rowIndex = Integer.parseInt(attributes.getValue("row"));
				cell = new Cell(sheet, sheet.new Position(colIndex, rowIndex));
				
				String fcolor = attributes.getValue("fcolor");
				if ( fcolor != null ) {
					cell.setfColor(new Color(Integer.parseInt(fcolor)));
				}
				
				String bcolor = attributes.getValue("bcolor");
				if ( bcolor != null ) {
					cell.setbColor(new Color(Integer.parseInt(bcolor)));
				}
				
				// Be sure to make the expression this way, so
				// a Null pointer exception is not thrown when the
				// attribute for underlined is not set
				if ("true".equalsIgnoreCase(attributes.getValue("underlined")))
					cell.setUnderlined(true);
				if ("true".equalsIgnoreCase(attributes.getValue("italic")))
					cell.setItalic(true);
				if ("true".equalsIgnoreCase(attributes.getValue("bold")))
					cell.setBold(true);
			}
		}

		@Override
		public void endElement(String uri, String localName, String name)
				throws SAXException {
			if (name.equalsIgnoreCase("CELL")) {
				cell.setInput(content.toString());
			} else if (name.equalsIgnoreCase("SPREADSHEET") || name.equalsIgnoreCase("WORKBOOK")){
				sheetHandler.endElement(uri, localName, name);
				reader.setContentHandler(sheetHandler);
			}
		}
	}
}
