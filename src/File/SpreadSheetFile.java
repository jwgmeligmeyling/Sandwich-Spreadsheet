package File;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

public class SpreadSheetFile {
	
	private ArrayList<Sheet> sheets; 
	
	/**
	 * Constructor voor geopende en geparste (xml) file.
	 */
	public SpreadSheetFile() {
		sheets = new ArrayList<Sheet>();
	}
	
	
	

	public void addSheet(Sheet newSheet) {
		sheets.add(newSheet);
	}
	
	public Sheet newSheet(String nameIn) {
		Sheet newSheet = new Sheet(nameIn);
		addSheet(newSheet);
		return newSheet;
	}
	
	public ArrayList<Sheet> getSheets() {
		return sheets;
	}
	
	public int countSheets() {
		return sheets.size();
	}
	
	public Sheet getSheet(int index) {
		return sheets.get(index);
	}
	
	
	/**
	 * Reads xml file
	 * @throws IOException 
	 * @throws SAXException 
	 * @throws ParserConfigurationException 
	 */
	public void openFile(String filename, String filepath) throws ParserConfigurationException, SAXException, IOException {
		for(int i = 0; i < 10; i++){
			XMLRead.read(filepath + "/" + filename);
		}
		
	}
	
	/**
	 * Writes xml file
	 * @param filename
	 * @param filepath
	 * @throws IOException 
	 * @throws FactoryConfigurationError 
	 * @throws XMLStreamException 
	 */
	public void saveFile(String filename, String filepath) throws XMLStreamException, FactoryConfigurationError, IOException {
		
		for(int i = 0; i < sheets.size(); i++){
			write(filepath+"/"+filename);
		}
		
	}
	
	/**
	 * XML Handler for SAX Parsing of Sheets
	 * @author Jim Hommes
	 *
	 */
	public static class XMLHandler extends DefaultHandler {
		private final XMLReader reader;
		private final Sheet sheet;

		/**
		 * Constructor for sheet parser
		 * @param sheet
		 * @param reader
		 */
		public XMLHandler(Sheet sheet, XMLReader reader) {
			this.reader = reader;
			this.sheet = sheet;
		}
		
		@Override
		public void startElement(String uri, String localName, String name,
				Attributes attributes) throws SAXException {
			
			if (name.equalsIgnoreCase("SPREADSHEET")) {
				String sheetName = attributes.getValue("name");
				
				if ( sheetName == null ) {
					sheet.setSheetName(sheetName);
				}
			} else if (name.equalsIgnoreCase("CELL")) {
				DefaultHandler cellHandler = new Cell.XMLHandler(sheet);
				reader.setContentHandler(cellHandler);
				cellHandler.startElement(uri, localName, name, attributes);
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
	public void write(String path) throws XMLStreamException,
			FactoryConfigurationError, IOException {
		OutputStream output = new FileOutputStream(new File(path));
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
	
}
