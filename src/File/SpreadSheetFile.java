package File;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

public class SpreadSheetFile {
	
	private ArrayList<Sheet> sheets = new ArrayList<Sheet>();
	
	private File file;
	private String name;
	
	/**
	 * Create a new empty Spreadsheet file based
	 */
	public SpreadSheetFile() {
		name = "Untitled.xml";
	}
	
	public SpreadSheetFile(Sheet sheet) {
		this();
		sheets.add(sheet);
	}
	
	/**
	 * Method to read from an XML file
	 * @param path
	 *            to XML-file
	 * @return <code>Sheet</code> parsed from XML-file
	 * @throws ParserConfigurationExceptionif
	 *             a parser cannot be created which satisfies the requested
	 *             configuration.
	 * @throws SAXException
	 *             for SAX errors.
	 * @throws ParserConfigurationException 
	 * @throws IOException
	 *             If any IO errors occur.
	 */
	public SpreadSheetFile(File file) throws SAXException, ParserConfigurationException, IOException {
		/*
		 * Volgens de instructies van SAX dienen we eerst een SAXParser aan te
		 * maken die ontstaat vanuit een SAXParser factory. De SAX zal alleen
		 * het uiteindelijke werk verichten dmv een handler met alle
		 * instructies. De handler is dus om te buigen naar onze hand.
		 */
		this.file = file;
		this.name = file.getName();
		SAXParserFactory factory = SAXParserFactory.newInstance();
		SAXParser saxParser = factory.newSAXParser();
		DefaultHandler handler = new SpreadSheetFile.XMLHandler(this, saxParser.getXMLReader());
		saxParser.parse(file, handler);
		init();
	}
	
	/**
	 * Calculate the values for the cells
	 */
	private void init() {
		for ( Sheet sheet : sheets ) {
			sheet.init();
		}
	}
	
	/**
	 * Append a sheet to this spreadsheet file
	 * @param sheet
	 */
	public void addSheet(Sheet sheet) {
		sheets.add(sheet);
	}
	
	/**
	 * @return get the sheets
	 */
	public ArrayList<Sheet> getSheets() {
		return sheets;
	}
	
	/**
	 * @return the path
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * @return the file
	 */
	public File getFile() {
		return file;
	}
	
	/**
	 * @return a new {@code Sheet} instance
	 */
	public Sheet createSheet() {
		Sheet sheet = new Sheet("Werkblad " + countSheets());
		addSheet(sheet);
		return sheet;
	}
	
	/**
	 * @return amount of {@code Sheet} instances in this workbook
	 */
	public int countSheets() {
		return sheets.size();
	}
	
	/**
	 * @param index
	 * @return {@code Sheet} instance at given index, or {@code null} if none exists
	 */
	public Sheet getSheet(int index) {
		if ( index == -1 ) {
			index = 0;
		}
		return sheets.get(index);
	}
	
	/**
	 * XML Handler for SAX Parsing of Sheets
	 * @author Jim Hommes
	 *
	 */
	public static class XMLHandler extends DefaultHandler {
		private final XMLReader reader;
		private final SpreadSheetFile sheets;

		/**
		 * Constructor for sheet parser
		 * @param sheet
		 * @param reader
		 */
		public XMLHandler(SpreadSheetFile sheets, XMLReader reader) {
			this.reader = reader;
			this.sheets = sheets;
		}
		
		@Override
		public void startElement(String uri, String localName, String name,
				Attributes attributes) throws SAXException {
					
			if (name.equalsIgnoreCase("SPREADSHEET")) {
				DefaultHandler sheetHandler = new Sheet.XMLHandler(sheets, reader, this);
				reader.setContentHandler(sheetHandler);
				sheetHandler.startElement(uri, localName, name, attributes);
			}
			
		}	
	}
	
	/**
	 * The function that writes the sheet to a XML file.
	 * <div><b>Author:</b><br>
	 * <ul>
	 * <li>Jim Hommes</li>
	 * </ul>
	 * </div>
	 * 
	 * @param path
	 *            Path to file
	 * @throws XMLStreamException
	 *             If there was an error occurred writing XML
	 * @throws FactoryConfigurationError
	 *             if an instance of this factory cannot be loaded
	 * @throws IOException
	 *             If there was an error writing the file in the correct
	 *             encoding
	 */
	public void write(File file) throws XMLStreamException,
			FactoryConfigurationError, IOException {
		OutputStream output = new FileOutputStream(file);
		XMLStreamWriter writer = XMLOutputFactory.newInstance()
				.createXMLStreamWriter(new OutputStreamWriter(output, "UTF-8"));
		
		writer.writeStartElement("WORKBOOK");
		
		for(int i = 0; i < sheets.size();i++){
			sheets.get(i).write(writer);
		}
		
		writer.writeEndElement();
		
		writer.close();
		output.close();
	}
	
	@Override
	public boolean equals(Object other){
		if(other instanceof SpreadSheetFile){
			SpreadSheetFile sheets = (SpreadSheetFile) other;
			return sheets.sheets.equals(this.sheets);
		}
		return false;
	}
	
	@Override
	public String toString(){
		return sheets.toString();
	}

	/**
	 * Get the index of the sheet in this workbook
	 */
	public int indexOf(Sheet sheet) {
		return sheets.indexOf(sheet);
	}
}
