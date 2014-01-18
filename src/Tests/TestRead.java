package Tests;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import org.junit.Test;
import org.xml.sax.SAXException;

import File.Cell;
import File.Sheet;
import File.Workbook;

public class TestRead {
	
	public String path;
	
	@Test
	public void TestReadNormalWholeBlock() throws ParserConfigurationException, SAXException, IOException{
		Workbook sheets = new Workbook(new File("xml/oude xml.xml"));
		Sheet sheet = sheets.getSheet(0);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		Cell[] lijst = sheet.getRange(1,1,2,3).getCellArray();
		Cell[] lijst2 = csheet.getRange(1,1,2,3).getCellArray();
		
		assertArrayEquals(lijst2, lijst);
	}

	@Test
	public void TestReadNormalSmallestBlock() throws ParserConfigurationException, SAXException, IOException{
		Workbook sheets = new Workbook(new File("xml/oude xml.xml"));
		Sheet sheet = sheets.getSheet(0);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		Cell[] lijst = sheet.getRange(1,1,2,1).getCellArray();
		Cell[] lijst2 = csheet.getRange(1,1,2,1).getCellArray();
		
		assertArrayEquals(lijst2, lijst);
	}
	
	@Test
	public void TestReadNormalSpecificBlock() throws ParserConfigurationException, SAXException, IOException{
		Workbook sheets = new Workbook(new File("xml/oude xml.xml"));
		Sheet sheet = sheets.getSheet(0);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		Cell[] lijst = sheet.getRange(2,2,1,3).getCellArray();
		Cell[] lijst2 = csheet.getRange(2,2,1,3).getCellArray();
		
		assertArrayEquals(lijst2, lijst);
	}
	
}
