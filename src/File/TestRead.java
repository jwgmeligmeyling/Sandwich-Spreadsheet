package File;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;

import File.Cell;

public class TestRead {
	
	public String path;
	
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
				check = lijst[i].getInput().equals(lijst2[i].getInput());
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
				check = lijst[i].getInput().equals(lijst2[i].getInput());
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
				check = lijst[i].getInput().equals(lijst2[i].getInput());
			}
		}
		
		assertEquals(check,true);
	}
	
	@Test
	public void TestReadFoutWholeBlock(){
		path = "xml/fout.xml";
		Sheet sheet = XMLRead.read(path);
		
		Sheet csheet = new Sheet();
		
		csheet.createCell(" I wonder how you handle\nline breaks in a cell?",4,4);
		csheet.createCell("Are doubly defined cells a problem for you?",1,10);
		csheet.createCell("It appears you show the first one, when a cell is doubly defined.",2,10);
		csheet.createCell("It appears you show the second one, when a cell is doubly defined.",2,10);
		csheet.createCell("If I use encoded symbols such as < and > or &, how do they show?",1,2);
		csheet.createCell("<VALUE>What if I define another xml tag in here?</VALUE>Or will only the text 'outside' of 'VALUE' show?",2,2);
		
		boolean check = true;
		
		Cell[] lijst = sheet.getRange(1,1,10,10).getCellArray();
		Cell[] lijst2 = csheet.getRange(1,1,10,10).getCellArray();
		
		System.out.println("bla"+sheet.getCellAt(2, 2).getInput());
		
		for(int i = 0; i < lijst.length; i++){
			if(check){
				check = lijst[i].getInput().equals("swag");
			}
		}
		
		assertEquals(check,true);
		
		
	}
}