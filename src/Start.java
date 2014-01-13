import File.SpreadSheetFile;

public class Start {
	public static void main(String[] args) {
		SpreadSheetFile spreadsheet = new SpreadSheetFile();
		new GUI.Window("Sandwich Spreadsheet",spreadsheet);
	}
}