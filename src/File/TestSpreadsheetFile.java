package File;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.junit.Before;
import org.junit.Test;
import org.xml.sax.SAXException;

public class TestSpreadsheetFile {
	
	public SpreadSheetFile sheets;
	public SpreadSheetFile sheets2;
	public Sheet sheet1;
	public Sheet sheet2;
	public Sheet sheet3;
	
	
	
	@Before
	public void voorbereidingen(){
		sheets = new SpreadSheetFile();
		sheets2 = new SpreadSheetFile();
		
		sheet1 = new Sheet();
		sheet1.createCell("Hallo! Cell 1,1 Sheet 1", 1, 1);
		sheet2 = new Sheet();
		sheet2.createCell("Hallo! Cell 2,2, Sheet 2",2,2);
		sheet3 = new Sheet();
		sheet3.createCell("Hallo! Cell 1,1, Sheet 3",1,1);
		sheet3.createCell("Hallo! Cell 2,2, Sheet 3",2,2);
		
		sheets.addSheet(sheet1);
		sheets.addSheet(sheet2);
		sheets.addSheet(sheet3);
	}
	
	@Test
	public void testSaveFile() throws XMLStreamException, FactoryConfigurationError, IOException {
		sheets.saveFile("output.xml", "xml");
		BufferedReader reader = new BufferedReader(new FileReader("xml/output.xml"));
		String one = reader.readLine();
		
		reader = new BufferedReader(new FileReader("xml/testvoorbeeld.xml"));
		String two = reader.readLine();
		
		System.out.println(one);
		System.out.println(two);
		
		assertEquals(one.equals(two),true);
	}
	
	@Test
	public void testReadFile() throws ParserConfigurationException, SAXException, IOException, XMLStreamException, FactoryConfigurationError{
		sheets2.openFile("output.xml", "xml");
		sheets2.saveFile("output.xml", "xml");
	}

}
