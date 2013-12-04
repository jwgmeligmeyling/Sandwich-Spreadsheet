import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;


import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Dit is de XML Parser. Hiervoor maken we gebruik van SAX (sax2r2.jar)
 * 
 * @author Jim Hommes
 * 
 */
public class XMLRead extends DefaultHandler{
	
	public static void main(String[] args) {
		
		try {
			/**
			 * Volgens de instructies van SAX dienen we eerst een SAXParser aan
			 * te maken die ontstaat vanuit een SAXParser factory. De SAX zal
			 * alleen het uiteindelijke werk verichten dmv een handler met alle
			 * instructies. De handler is dus om te buigen naar onze hand.
			 */
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();

			
			DefaultHandler handler = new DefaultHandler() {
				
				boolean bcell = false;
				
				/**
				 * Start Element print het soort element af. De inhoud van deze
				 * cell is dus SPREADSHEET of CELL, etc. Deze inhoud hoeven we
				 * niet te gebruiken maar is noodzakelijk voor de werking.
				 */
				public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

					System.out.println("Start Element:  " + qName);

					int length = attributes.getLength();

					for (int i = 0; i < length; i++) {
						String attrname = attributes.getQName(i);
						System.out.println("Attr. Name: " + attrname);
						String value = attributes.getValue(i);
						System.out.println("Value: "+value);	
					}
					
					if (qName.equalsIgnoreCase("CELL")) {
						bcell = true;
					}

				}


				/**
				 * Deze functie print de inhoud van het element. De String
				 * inhoud neemt de opgegeven array van chars, en vormt zo de
				 * string met het antwoord. Dit dienen wij te gebruiken om
				 * verder te gaan.
				 */
				public void characters(char ch[], int start, int length) throws SAXException {
					if (bcell) {
						String inhoud = new String(ch, start, length);
						System.out.println("CELL: " + inhoud);
						bcell = false;
					}
				}
				
				/**
				 * Print het einde van het element.
				 */
				public void endElement(String uri, String localName, String qName) throws SAXException{
					if(qName.equalsIgnoreCase("CELL")){
						System.out.println("End Element: " + qName);
					}
					
				}
			};
			
			saxParser.parse("xml/fout.xml",handler);
			
		} catch (SAXException e) {
			System.out.println("Unhandled SAXException");
		} catch (ParserConfigurationException e) {
			System.out.println("Unhandled ParseConfigurationException");
		} catch (IOException e) {
			System.out.println("Unhandled IOException");
		}
	}
}
