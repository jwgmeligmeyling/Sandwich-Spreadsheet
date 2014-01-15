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
	private SpreadSheetFile newFile;
	
	/**
	 * Consturctor for the GUI
	 */
	public Window() {
		this(new SpreadSheetFile(new Sheet()));
	}
	
	/**
	 * Constructor for the GUI.
	 * @param title is the title of the window.
	 * @throws HeadlessException
	 */
	public Window(SpreadSheetFile spreadsheet) throws HeadlessException {
		super("Sandwich Spreadsheet - " + spreadsheet.getName());
		newFile = spreadsheet;
		
		setSize(800, 450);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		tbMain = new SToolbar(this);
		tabbedPane = new JTabbedPane();
		statusBar = new SStatusBar(this);
		formule = new FormuleBalk();
		SMenuBar smenubar = new SMenuBar(this);
		setJMenuBar(smenubar);
		tbMain.add(formule);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		container.add(tbMain, BorderLayout.PAGE_START);
		container.add(statusBar, BorderLayout.PAGE_END);
		container.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
		
		tbMain.createSelectionListener(getCurrentTable());
		paintSheets();
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
		getCurrentTable().updateUI();
	}
	
	public Sheet createSheet() {
		return newFile.createSheet();
	}
	
	/**
	 * @return The selected {@code  Range} or null if no selection
	 */
	public Range getSelectedRange() {
		return getCurrentTable().getSelectedRange();
	}
	
	/**
	 * @return get the selected cell, returns null if no selected cell or
	 *         multiple selected cell
	 */
	public Cell getSelectedCell() {
		Range range = getSelectedRange();
		if ( range != null &&  range.isSingleCell() ) {
			return range.firstCell();
		} else {
			return null;
		}
	}
	
	/**
	 * Go to a {@code Sheet}
	 * @param sheet
	 */
	public void goToSheet(Sheet sheet) {
		tabbedPane.setSelectedIndex(newFile.indexOf(sheet));
	}
	
	/**
	 * Construct (paint) all tabs for the sheets in the current Workbook
	 */
	private void paintSheets() {
		for(Sheet sheet: newFile.getSheets()){
			paintSheet(sheet);
		}
	}
	
	/**
	 * Construct (paint) a tab in the tabbed pane for a new Sheet
	 * @param sheet
	 */
	public void paintSheet(Sheet sheet) {
		STable table = new STable(sheet, formule);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		box.add(new JScrollPane(table));
		
		tabbedPane.addTab(sheet.getSheetName(), box);
	}

	/**
	 * @return the {@code JTabbedPane} for this {@code Window} such that
	 * the actions can access it.
	 */
	public JTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	/**
	 * Set tab placement
	 * @param setting
	 */
	public void setTabPlacement(int setting) {
		tabbedPane.setTabPlacement(setting);
	}
	
	public JLabel getStatusBar() {
		return statusBar;
	}
	
	public void setStatusBarVisibility(boolean visible) {
		statusBar.setVisible(visible);
	}
	
	public JTextField getCurrentEditor(){
		return (JTextField) getCurrentTable().getEditorComponent();
	}	
	
	public static void main(String[] args) {
		new Window();
	}

}