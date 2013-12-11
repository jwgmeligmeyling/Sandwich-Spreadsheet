package File;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;

import File.Cell;

public class TestRead {
	
	public String path;
	
	@Before
	public void create(){
		
	}
	
	@Test
	public void TestReadNormalWholeBlock(){
		path = "xml/oude xml.xml";
		Sheet sheet = XMLRead.read(path);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		Cell[] lijst = sheet.getRange(1,1,2,3).getCellArray();
		Cell[] lijst2 = csheet.getRange(1,1,2,3).getCellArray();
		
		
		//Testing the whole block of cells
		boolean check = true;
		for(int i = 0; i < lijst.length; i++){
			if(check){
				check = lijst[i].getValue().equals(lijst2[i].getValue());
			}
		}
		
		assertEquals(check,true);	
			
		//Testing the block from 1,1 to 1,2
		check = true;
		
		lijst = sheet.getRange(1,1,2,1).getCellArray();
		lijst2 = csheet.getRange(1,1,2,1).getCellArray();
		
		for(int i = 0; i < lijst.length; i++){
			if(check){
				check = lijst[i].getValue().equals(lijst2[i].getValue());
			}
		}
		
		assertEquals(check,true);
		
	}

	@Test
	public void TestReadNormalSmallestBlock(){
		path = "xml/oude xml.xml";
		Sheet sheet = XMLRead.read(path);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		boolean check = true;
		
		Cell[] lijst = sheet.getRange(1,1,2,1).getCellArray();
		Cell[] lijst2 = csheet.getRange(1,1,2,1).getCellArray();
		
		for(int i = 0; i < lijst.length; i++){
			if(check){
				check = lijst[i].getValue().equals(lijst2[i].getValue());
			}
		}
		
		assertEquals(check,true);
		
	}
	
	@Test
	public void TestReadNormalSpecificBlock(){
		path = "xml/oude xml.xml";
		Sheet sheet = XMLRead.read(path);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell("IF(B1+5,10,6)",1,1);
		csheet.createCell("Inhoud 1,2",2,1);
		csheet.createCell("Inhoud 2,1",1,2);
		csheet.createCell("Inhoud 2,2",2,2);
		csheet.createCell("Inhoud 3,1",1,3);
		csheet.createCell("Inhoud 3,2",2,3);
		
		boolean check = true;
		
		Cell[] lijst = sheet.getRange(2,2,1,3).getCellArray();
		Cell[] lijst2 = csheet.getRange(2,2,1,3).getCellArray();
		
		for(int i = 0; i < lijst.length; i++){
			if(check){
				check = lijst[i].getValue().equals(lijst2[i].getValue());
			}
		}
		
		assertEquals(check,true);
	}
}
