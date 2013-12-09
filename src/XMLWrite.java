import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public class XMLWrite {
	
	public static void main(String[] args){
		try{
			OutputStream output = new FileOutputStream(new File("xml/output.xml")); 
			XMLStreamWriter out = XMLOutputFactory.newInstance().createXMLStreamWriter(new OutputStreamWriter(output,"UTF-8"));
			
			out.writeStartDocument("SPREADSHEET");
			out.writeStartElement("CELL");
			
			out.writeCharacters("Hallo! ik ben een cell");
			
			out.writeEndElement();
			out.writeEndDocument();
			
		}catch(FileNotFoundException e){
			System.out.println("File not found!");
		}catch(XMLStreamException e){
			System.out.println("XML Stream exception");
		}catch(UnsupportedEncodingException e){
			System.out.println("Unsupported encoding exception");
		}
	}
}