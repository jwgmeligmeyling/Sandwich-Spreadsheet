package File;
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
public class XMLRead {
	
	public static Sheet read(String path) {
		
		try {
			/**
			 * Volgens de instructies van SAX dienen we eerst een SAXParser aan
			 * te maken die ontstaat vanuit een SAXParser factory. De SAX zal
			 * alleen het uiteindelijke werk verichten dmv een handler met alle
			 * instructies. De handler is dus om te buigen naar onze hand.
			 */
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			
			final Sheet sheet = new Sheet();
			final String[] attribvalues = new String[3];
			
			final StringBuilder inhoud = new StringBuilder();
			
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
					
					//Indien "type" leeg is
					if(length < 3){
						attribvalues[2] = "";
					}
					
					//De attributen ophalen en in de final array attribvalues plaatsen
					for (int i = 0; i < length; i++) {
						String attrname = attributes.getQName(i);
						System.out.println("Attr. Name:     " + attrname);
						String value = attributes.getValue(i);
						System.out.println("Value:          " + value);
						attribvalues[i] = value;
					}
					
					//Als het element een CELL is, zet de boolean op true en laat inlezen
					if (qName.equalsIgnoreCase("CELL")) {
						bcell = true;
						//sheet.createCell("",Integer.parseInt(attribvalues[0]),Integer.parseInt(attribvalues[1]));
					}
				}

				/**
				 * Deze functie print de inhoud van het element. De String
				 * inhoud neemt de opgegeven array van chars, en vormt zo de
				 * string met het antwoord. De Stringbuilder neemt al deze onderdelen
				 * en maakt zo de inhoud.
				 */
				public void characters(char ch[], int start, int length) throws SAXException {
					if (bcell) {
						String inhoudcell = new String(ch, start, length);
						System.out.println("CELL:           " + inhoudcell);
						inhoud.append(inhoudcell);
					}
				}
				
				/**
				 * Print het einde van het element en print de uiteindelijke String.
				 */
				public void endElement(String uri, String localName, String qName) throws SAXException {
					if(qName.equalsIgnoreCase("CELL")){
						bcell = false;
						sheet.createCell(inhoud.toString(),Integer.parseInt(attribvalues[1]),Integer.parseInt(attribvalues[0]));
						System.out.println("Final Cell:     "+inhoud.toString());
					}
					
					inhoud.setLength(0);
					System.out.println("End Element:    " + qName);
					
				}
			};

			saxParser.parse(path, handler);
			
			return sheet;

		} catch (SAXException e) {
			System.out.println("Unhandled SAXException");
		} catch (ParserConfigurationException e) {
			System.out.println("Unhandled ParseConfigurationException");
		} catch (IOException e) {
			System.out.println("Unhandled IOException");
		}
		return null;
	}
}
