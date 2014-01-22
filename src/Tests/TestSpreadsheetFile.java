package Tests;

import static org.junit.Assert.*;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.junit.Before;
import org.junit.Test;
import org.xml.sax.SAXException;

import File.Sheet;
import File.Workbook;

public class TestSpreadsheetFile {
	
	public Workbook sheets;
	public Workbook sheets2;
	public Sheet sheet1;
	public Sheet sheet2;
	public Sheet sheet3;
	
	@Before
	public void voorbereidingen(){
		sheets = new Workbook();
		sheets2 = new Workbook();
		
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
	
	@SuppressWarnings("resource")
	@Test
	public void testSaveFile() throws XMLStreamException, FactoryConfigurationError, IOException {
		sheets.write(new File("xml/output.xml"));
		BufferedReader reader = new BufferedReader(new FileReader("xml/output.xml"));
		String one = reader.readLine();
		String two = "<WORKBOOK><SPREADSHEET name=\"New sheet\"><CELL row=\"1\" column=\"1\" type=\"TEXT\">Hallo! Cell 1,1 Sheet 1</CELL></SPREADSHEET><SPREADSHEET name=\"New sheet\"><CELL row=\"2\" column=\"2\" type=\"TEXT\">Hallo! Cell 2,2, Sheet 2</CELL></SPREADSHEET><SPREADSHEET name=\"New sheet\"><CELL row=\"1\" column=\"1\" type=\"TEXT\">Hallo! Cell 1,1, Sheet 3</CELL><CELL row=\"2\" column=\"2\" type=\"TEXT\">Hallo! Cell 2,2, Sheet 3</CELL></SPREADSHEET></WORKBOOK>";
		assertEquals(one, two);
	}
	
	
	@Test
	public void testReadFileMultipleSheets() throws ParserConfigurationException, SAXException, IOException, XMLStreamException, FactoryConfigurationError{
		Workbook file = new Workbook();
		Sheet sheet = new Sheet();
		Sheet sheet2 = new Sheet();
		
		sheet.createCell("test", 1, 1);
		sheet.createCell("test2",2,2);
		
		sheet2.createCell("test3",3,3);
		sheet2.createCell("test4", 4, 4);
		
		file.addSheet(sheet);
		file.addSheet(sheet2);
		
		Workbook file2 = new Workbook(new File("xml/Test XML bestanden (Niet aankomen)/test2.xml"));
		
		assertEquals(file, file2);
	}
	
	@Test
	public void testRead() throws ParserConfigurationException, SAXException, IOException{
		Workbook file = new Workbook();
		Sheet sheet = new Sheet();
		
		file.addSheet(sheet);
		
		sheet.createCell("test", 1, 1);
		
		Workbook file2 = new Workbook(new File("xml/Test XML bestanden (Niet aankomen)/test.xml"));
		
		assertEquals(file,file2);
		
	}
	
	@Test
	public void TestReadAttributes() throws SAXException, ParserConfigurationException, IOException{
		Workbook sheets = new Workbook(new File("xml/Test XML bestanden (Niet aankomen)/test3.xml"));
		Sheet sheet = sheets.getSheet(0);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("BC blue en FC red",1,2);
		csheet.getCellAt(1,2).setbColor(new Color(-16776961));
		csheet.getCellAt(1,2).setfColor(new Color(-65536));
		
		csheet.createCell("Lekker bold",2,2);
		csheet.getCellAt(2,2).setBold(true);
		
		csheet.createCell("Lekker italic",3,2);
		csheet.getCellAt(3,2).setItalic(true);
		
		csheet.createCell("Lekker underlined", 4,2);
		csheet.getCellAt(4,2).setUnderlined(true);
		
		Workbook csheets = new Workbook();
		csheets.addSheet(csheet);
		
		assertEquals(sheets,csheets);
	}
	
	@Test
	public void testWriteAttributes() throws ParserConfigurationException, SAXException, IOException, XMLStreamException, FactoryConfigurationError{
		Sheet csheet = new Sheet();
		
		csheet.createCell("BC blue en FC red",1,2);
		csheet.getCellAt(1,2).setbColor(new Color(-16776961));
		csheet.getCellAt(1,2).setfColor(new Color(-65536));
		
		csheet.createCell("Lekker bold",2,2);
		csheet.getCellAt(2,2).setBold(true);
		
		csheet.createCell("Lekker italic",3,2);
		csheet.getCellAt(3,2).setItalic(true);
		
		csheet.createCell("Lekker underlined", 4,2);
		csheet.getCellAt(4,2).setUnderlined(true);
		
		Workbook csheets = new Workbook();
		csheets.addSheet(csheet);
		
		csheets.write(new File("xml/Test XML bestanden (Niet aankomen)/test4.xml"));
		
		BufferedReader reader = new BufferedReader(new FileReader("xml/Test XML bestanden (Niet aankomen)/test4.xml"));
		String one = reader.readLine();
		String two = "<WORKBOOK><SPREADSHEET name=\"New sheet\"><CELL row=\"2\" column=\"4\" type=\"TEXT\" underlined=\"true\">Lekker underlined</CELL><CELL row=\"2\" column=\"2\" type=\"TEXT\" bold=\"true\">Lekker bold</CELL><CELL row=\"2\" column=\"3\" type=\"TEXT\" italic=\"true\">Lekker italic</CELL><CELL row=\"2\" column=\"1\" type=\"TEXT\" fcolor=\"-65536\" bcolor=\"-16776961\">BC blue en FC red</CELL></SPREADSHEET></WORKBOOK>";
		
		assertEquals(one,two);
		
		reader.close();
	}
	
}
