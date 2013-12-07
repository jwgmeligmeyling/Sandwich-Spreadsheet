package File;

import java.util.ArrayList;

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
	
	public void newSheet() {
		
	}
	
	
	
	/**
	 * Reads xml file
	 */
	public static void openFile() {
		
	}
	
	/**
	 * Writes xml file
	 * @param filename
	 * @param filepath
	 */
	public void saveFile(String filename, String filepath) {
		
	}
}
