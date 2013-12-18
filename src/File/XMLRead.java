package File;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Dit is de XML Parser. Hiervoor maken we gebruik van SAX (sax2r2.jar)
 * 
 * @author Jim Hommes
 * 
 */
public class XMLRead {
	
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
	 * @throws IOException
	 *             If any IO errors occur.
	 */
	public static Sheet read(String path) throws ParserConfigurationException,
			SAXException, IOException {
		/*
		 * Volgens de instructies van SAX dienen we eerst een SAXParser aan te
		 * maken die ontstaat vanuit een SAXParser factory. De SAX zal alleen
		 * het uiteindelijke werk verichten dmv een handler met alle
		 * instructies. De handler is dus om te buigen naar onze hand.
		 */
		SAXParserFactory factory = SAXParserFactory.newInstance();
		SAXParser saxParser = factory.newSAXParser();
		Sheet sheet = new Sheet();
		
		DefaultHandler handler = new SpreadSheetFile.XMLHandler(sheet,
				saxParser.getXMLReader());
		saxParser.parse(path, handler);

		return sheet;
	}
}
