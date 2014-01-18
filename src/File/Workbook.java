package File;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

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

/**
 * The class for the {@code Workbook}. A workbook may contain several {@code Sheets}.
 * A workbook can be bound to a {@code File} in the system.
 * @author Jan-Willem Gmelig Meyling
 * @author Maarten Flikkema
 * @Author Jim Hommes
 */
public class Workbook implements Interfaces.Workbook {

	private File file;
	private String name;
	private final ArrayList<Sheet> sheets = new ArrayList<Sheet>();
	

	/**
	 * Create a new empty workbook with the name Untitled.xml
	 */
	public Workbook() {
		name = "Untitled.xml";
	}
	
	/**
	 * Create a new workbook with a sheet appended to it
	 * @param sheet
	 */
	public Workbook(Sheet sheet) {
		this();
		sheets.add(sheet);
	}
	
	/**
	 * Method to read from an XML file
	 * @param file
	 * @throws ParserConfigurationException
	 * 			   If a parser cannot be created which satisfies the requested
	 *             configuration.
	 * @throws SAXException for SAX errors.
	 * @throws ParserConfigurationException 
	 * @throws IOException If any IO errors occur.
	 */
	public Workbook(File file) throws SAXException, ParserConfigurationException, IOException {
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
		DefaultHandler handler = new Workbook.XMLHandler(this, saxParser.getXMLReader());
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
	
	@Override
	public void addSheet(Sheet sheet) {
		sheets.add(sheet);
	}
	
	@Override
	public List<Sheet> getSheets() {
		return sheets;
	}
	
	@Override
	public String getName() {
		return name;
	}
	
	@Override
	public File getFile() {
		return file;
	}
	
	@Override
	public Sheet createSheet() {
		Sheet sheet = new Sheet("Werkblad " + countSheets());
		addSheet(sheet);
		return sheet;
	}
	
	@Override
	public int countSheets() {
		return sheets.size();
	}
	
	@Override
	public Sheet getSheet(int index) {
		if ( index == -1 ) {
			index = 0;
		}
		return sheets.get(index);
	}
	
	@Override
	public int indexOf(Sheet sheet) {
		return sheets.indexOf(sheet);
	}

	@Override
	public void write(File file) throws XMLStreamException,
			FactoryConfigurationError, IOException {
		OutputStream output = new FileOutputStream(file);
		XMLStreamWriter writer = XMLOutputFactory.newInstance().createXMLStreamWriter(new OutputStreamWriter(output, "UTF-8"));
		
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
		if(other instanceof Workbook){
			Workbook sheets = (Workbook) other;
			return sheets.sheets.equals(this.sheets);
		}
		return false;
	}
	
	@Override
	public String toString(){
		return sheets.toString();
	}

	/**
	 * XML Handler for SAX Parsing of Sheets
	 * @author Jim Hommes
	 *
	 */
	public static class XMLHandler extends DefaultHandler {
		private final XMLReader reader;
		private final Workbook sheets;
	
		/**
		 * Constructor for sheet parser
		 * @param sheets
		 * @param reader
		 */
		public XMLHandler(Workbook sheets, XMLReader reader) {
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
}
