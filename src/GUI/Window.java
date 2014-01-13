package GUI;

import javax.swing.*;

import java.awt.*;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;
import File.SpreadSheetFile;

@SuppressWarnings("serial")
public class Window extends JFrame {
	
	private SToolbar tbMain;
	private FormuleBalk formule;
	private JTabbedPane tabbedPane;
	private SStatusBar statusBar;
	
	private static SpreadSheetFile newFile;
	
	/**
	 * Constructor for the GUI.
	 * @param title is the title of the window.
	 * @throws HeadlessException
	 */
	public Window(String title, SpreadSheetFile spreadsheet) throws HeadlessException {
		super(title);
		
		setSize(800, 450);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		tbMain = new SToolbar(this);
		tabbedPane = new JTabbedPane();
		SMenuBar smenubar = new SMenuBar(this);
		setJMenuBar(smenubar);
		smenubar.setTabbedPane(tabbedPane);
		
		statusBar = new SStatusBar(this);
		formule = new FormuleBalk();
		tbMain.add(formule);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		container.add(tbMain, BorderLayout.PAGE_START);
		container.add(statusBar, BorderLayout.PAGE_END);
		container.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
		
		newFile = spreadsheet;
		createSheet();
		tbMain.createSelectionListener(getCurrentTable());
	}
	
	public Sheet getCurrentSheet() {
		return newFile.getSheet(tabbedPane.getSelectedIndex());
	}
	
	public SpreadSheetFile getCurrentSpreadSheetFile(){
		return newFile;
	}
	
	public STable getCurrentTable() {
		return getCurrentSheet().getSTable();
	}
	
	public void updateTable() {
		// TODO not sure if this is enough to update the table
		getCurrentTable().updateUI();
	}
	
	public Range getSelectedRange() {
		return getCurrentTable().getSelectedRange();
	}
	
	public Cell getSelectedCell() {
		Range range = getSelectedRange();
		if ( range.isSingleCell() ) {
			return range.firstCell();
		} else {
			return null;
		}
	}
	
	public void createSheet() {
		for(Sheet sheet: newFile.getSheets()){
			sheet.init();
			
			STable table = new STable(sheet, formule);
			
			Box box = Box.createVerticalBox();
			box.add(table.getTableHeader());
			JScrollPane scrollPane = new JScrollPane(table);
			scrollPane.setPreferredSize(new Dimension(700,500));
			box.add(scrollPane);
			
			tabbedPane.addTab(sheet.getSheetName(), box);
		
		}
		Sheet newSheet = newFile.newSheet("Sheet" + (newFile.countSheets() + 1));
		System.out.println("countSheets() = " + newFile.countSheets());
		
		//fillSheet(newSheet);
		
		STable table = new STable(newSheet, formule);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		JScrollPane scrollPane = new JScrollPane(table);
		scrollPane.setPreferredSize(new Dimension(700,500));
		box.add(scrollPane);
		
		tabbedPane.addTab(newSheet.getSheetName(), box);
	}
	
	private void fillSheet(Sheet sheet) {
		sheet.createCell("SOM:", 0, 0);
		sheet.createCell("=SUM(A2:T200)", 1, 0);
		sheet.createCell("=B1+E1", 2, 0);

		Cell X = sheet.createCell("COUNT:", 3, 0);
		X.setbColor(new Color(180, 180, 255));
		X.setBold(true);
		Cell Y = sheet.createCell("test", 5, 0);
		Y.setUnderlined(true);
		Y.setItalic(true);
		Y.setfColor(new Color(180,150,255));
		sheet.createCell("=COUNT(A2:T200)", 4, 0);
		sheet.createCell("COUNTIF>50:", 6, 0);
		sheet.createCell("=COUNTIF(A2:T200;\">=\"&50))", 7, 0);
		sheet.createCell("SUMIF>50:", 9, 0);
		sheet.createCell("=SUMIF(A2:T200,\">\"&50)", 10, 0);

		for (int i = 0; i < 20; i++) {
			for (int j = 1; j < 200; j++) {
				sheet.createCell("=RANDBETWEEN(0,100)", i, j);
			}
		}
	}
	
	public JTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	public void setTabPlacement(int setting) {
		tabbedPane.setTabPlacement(setting);
	}
	
	public JLabel getStatusBar() {
		return statusBar;
	}
	
	public void setStatusBarVisibility(boolean visible) {
		statusBar.setVisible(visible);
	}
	
	
	public static void main(String[] args) {
		SpreadSheetFile ssheet = new SpreadSheetFile();
		new Window("Sandwich Spreadsheet", ssheet);
	}
	
	
}